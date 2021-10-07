---
layout: post
title:  "Finding Resilience Issues with Filibuter: A Tutorial"
date:   2021-10-08 00:00:00 -0000
categories: filibuster
group: filibuster
---

_This is the third post in a series of posts on our upcoming ACM SoCC '21 paper, 
"Service-level Fault Injection Testing" leading up to the open-source release of our prototype implementation, Filibuster.  In our [first post](http://christophermeiklejohn.com/filibuster/2021/10/02/filibuster-1.html), we talked about the challenges of doing resilience engineering research on microservice architectures due to the lack of an application corpus; in our [second post](http://christophermeiklejohn.com/filibuster/2021/10/06/filibuster-2.html), we detailed our approach called Service-level Fault Injection Testing, or SFIT._

In this post, we're going to look at how we can use Filibuster to test a small microservice application composed of three 
services.  We *highly recommend* you read our [previous post](http://christophermeiklejohn.com/filibuster/2021/10/06/filibuster-2.html) that describes how precisely Service-level Fault 
Injection works: it's critical to understand the methodology before seeing it applied using our prototype implementation
called Filibuster.

## Introduction

In this tutorial, you will:

1. Create three Flask apps that work together to respond "foo bar baz" to a client (one of which contains a bug.)
2. Write functional tests for your Flask apps.
3. Use Filibuster to find the bug.
4. Fix the bug and verify resilience with Filibuster in your local, development environment.

Let's get started.

## Building the services.

Let's start by first implementing our services.  These will be standard Python microservices, 
implemented with Flask, with one minor modification: we've added additional instrumentation lines
at the top of each service so Filibuster can monitor the remote calls both sent and received by these services.

### Creating the `baz` service.

In ``filibuster-tutorial/service/baz/baz/app.py``, add the following code to implement the service.

```python
from flask import Flask, jsonify
from werkzeug.exceptions import ServiceUnavailable
import os
import sys

app = Flask(__name__)

## Instrument using filibuster

from filibuster.instrumentation.requests import RequestsInstrumentor as FilibusterRequestsInstrumentor
FilibusterRequestsInstrumentor().instrument(service_name="baz")

from filibuster.instrumentation.flask import FlaskInstrumentor as FilibusterFlaskInstrumentor
FilibusterFlaskInstrumentor().instrument_app(app, service_name="baz")

@app.route("/health-check", methods=['GET'])
def baz_health_check():
    return jsonify({ "status": "OK" })

@app.route("/baz", methods=['GET'])
def baz():
    return "baz"

if __name__ == "__main__":
    app.run(port=5002, host="0.0.0.0"))
```

Note the instrumentation code under ``## Instrument using filibuster``:

```python
from filibuster.instrumentation.requests import RequestsInstrumentor as FilibusterRequestsInstrumentor
FilibusterRequestsInstrumentor().instrument(service_name="baz")

from filibuster.instrumentation.flask import FlaskInstrumentor as FilibusterFlaskInstrumentor
FilibusterFlaskInstrumentor().instrument_app(app, service_name="baz")
```

Each service you create will need to include this code, with ``service_name`` updated accordingly. This instrumentation 
code allows Filibuster to instrument both ``flask`` and ``requests``, which in turn allows Filibuster to test
different fault combinations.

These are actually forks of the standard opentelemetry instrumentation for Python: the only change 
that you have to make if you are already using these, is change the source of the import and 
annotate the service name.

### Creating the `bar` service.

In ``filibuster-tutorial/service/bar/bar/app.py``, add the following code.

```python
from flask import Flask, jsonify
from werkzeug.exceptions import ServiceUnavailable
import requests
import os
import sys

app = Flask(__name__)

## Instrument using filibuster

from filibuster.instrumentation.requests import RequestsInstrumentor as FilibusterRequestsInstrumentor
FilibusterRequestsInstrumentor().instrument(service_name="bar")

from filibuster.instrumentation.flask import FlaskInstrumentor as FilibusterFlaskInstrumentor
FilibusterFlaskInstrumentor().instrument_app(app, service_name="bar")

@app.route("/health-check", methods=['GET'])
def bar_health_check():
    return jsonify({ "status": "OK" })

@app.route("/bar/baz", methods=['GET'])
def bar():
    try:
        response = requests.get("{}/baz".format("http://localhost:5002"), timeout=10)
    except requests.exceptions.ConnectionError:
        raise ServiceUnavailable("The baz service is unavailable.")
    except requests.exceptions.Timeout:
        raise ServiceUnavailable("The baz service timed out.")

    if response.status_code != 200:
        raise ServiceUnavailable("The baz service is malfunctioning.")

    return "bar " + response.text

if __name__ == "__main__":
    app.run(port=5001, host="0.0.0.0")
```

Finall, our last service.

### Creating the `foo` service.

In ``filibuster-tutorial/service/foo/foo/app.py``, add the following code.

```python
from flask import Flask, jsonify
from werkzeug.exceptions import ServiceUnavailable
import requests
import os
import sys

app = Flask(__name__)

## Instrument using filibuster

from filibuster.instrumentation.requests import RequestsInstrumentor as FilibusterRequestsInstrumentor
FilibusterRequestsInstrumentor().instrument(service_name="foo")

from filibuster.instrumentation.flask import FlaskInstrumentor as FilibusterFlaskInstrumentor
FilibusterFlaskInstrumentor().instrument_app(app, service_name="foo")

@app.route("/health-check", methods=['GET'])
def foo_health_check():
    return jsonify({ "status": "OK" })

@app.route("/foo/bar/baz", methods=['GET'])
def foo():
    try:
        response = requests.get("{}/bar/baz".format("http://localhost:5001"), timeout=10)
    except requests.exceptions.Timeout:
        raise ServiceUnavailable("The bar service timed out.")

    if response.status_code != 200:
        raise ServiceUnavailable("The bar service is malfunctioning.")

    return "foo " + response.text

if __name__ == "__main__":
    app.run(port=5000, host="0.0.0.0"))
```

We're done!  Now, let's write a functional test.

### Functional Test

Now that your Flask apps are created, write a functional test. This test will ensure that our three apps work 
together to return "foo bar baz" to a client. 

In ``filibuster-tutorial/functional/test_foo_bar_baz.py``, add the following code.

```python
#!/usr/bin/env python

import requests
import os
import sys

# Note that tests should be prefixed with test_functional for filibuster compatibility
def test_functional_foo_bar_baz():
    response = requests.get("{}/foo/bar/baz".format("http://localhost:5000"), timeout=10)
    assert response.status_code == 200 and response.text == "foo bar baz"

if __name__ == "__main__":
    test_functional_foo_bar_baz()
```

Now, let's verify that the functional test passes.  First, let's start the required services.  We'll 
use a little helper we will add to our `Makefile` to start the services using Python and wait
for them to come online.

```bash
cd filibuster-tutorial
make local-start
```

Now, run the functional test.

```bash
chmod 755 functionaal/test_foo_bar_baz.py
./functional/test_foo_bar_baz.py
```

At this point, the test should pass, but, did we properly account for the ways that our services' dependencies can fail?

## Finding the Bug

Let's use Filibuster to identify bugs using fault injection.  First, we can use Filibuster to identify bugs using a
default set of faults for the application.  To do this, we provide the Filibuster CLI tool with the path to the functional test.  If we don't specify what faults to inject, Filibuster will use test default set of common faults.

```shell
filibuster --functional-test ./functional/test_foo_bar_baz.py
```

We should see output like the following:

```python
    * Serving Flask app "filibuster.server" (lazy loading)
    * Environment: production
    WARNING: Do not use the development server in a production environment.
    Use a production WSGI server instead.
    * Debug mode: off
    * Running on all addresses.
    WARNING: This is a development server. Do not use it in a production deployment.
    * Running on http://100.68.79.169:5005/ (Press CTRL+C to quit)
127.0.0.1 - - [27/Sep/2021 10:35:05] "GET /health-check HTTP/1.1" 200 -
[FILIBUSTER] [NOTICE]: Running test ./functional/test_foo_bar_baz.py
[FILIBUSTER] [INFO]: Running initial non-failing execution (test 1) ./functional/test_foo_bar_baz.py
127.0.0.1 - - [27/Sep/2021 10:35:05] "GET /filibuster/new-test-execution/foo HTTP/1.1" 200 -
127.0.0.1 - - [27/Sep/2021 10:35:05] "PUT /filibuster/create HTTP/1.1" 200 -
127.0.0.1 - - [27/Sep/2021 10:35:05] "POST /filibuster/update HTTP/1.1" 200 -
127.0.0.1 - - [27/Sep/2021 10:35:05] "GET /filibuster/new-test-execution/bar HTTP/1.1" 200 -
127.0.0.1 - - [27/Sep/2021 10:35:05] "PUT /filibuster/create HTTP/1.1" 200 -
127.0.0.1 - - [27/Sep/2021 10:35:05] "POST /filibuster/update HTTP/1.1" 200 -
127.0.0.1 - - [27/Sep/2021 10:35:05] "POST /filibuster/update HTTP/1.1" 200 -
127.0.0.1 - - [27/Sep/2021 10:35:05] "POST /filibuster/update HTTP/1.1" 200 -
[FILIBUSTER] [INFO]: [DONE] Running initial non-failing execution (test 1)
[FILIBUSTER] [INFO]: Running test 2
[FILIBUSTER] [INFO]: Total tests pruned so far: 0
[FILIBUSTER] [INFO]: Total tests remaining: 9
[FILIBUSTER] [INFO]:
[FILIBUSTER] [INFO]: =====================================================================================
[FILIBUSTER] [INFO]: Test number: 2
[FILIBUSTER] [INFO]:
[FILIBUSTER] [INFO]: gen_id: 0
[FILIBUSTER] [INFO]:   module: requests
[FILIBUSTER] [INFO]:   method: get
[FILIBUSTER] [INFO]:   args: ['5001/bar/baz']
[FILIBUSTER] [INFO]:   kwargs: {}
[FILIBUSTER] [INFO]:   vclock: {'foo': 1}
[FILIBUSTER] [INFO]:   origin_vclock: {}
[FILIBUSTER] [INFO]:   execution_index: [["b13f73ac8ced79cb093a638972923de1", 1]]
[FILIBUSTER] [INFO]:
[FILIBUSTER] [INFO]: gen_id: 1
[FILIBUSTER] [INFO]:   module: requests
[FILIBUSTER] [INFO]:   method: get
[FILIBUSTER] [INFO]:   args: ['5002/baz']
[FILIBUSTER] [INFO]:   kwargs: {}
[FILIBUSTER] [INFO]:   vclock: {'foo': 1, 'bar': 1}
[FILIBUSTER] [INFO]:   origin_vclock: {'foo': 1}
[FILIBUSTER] [INFO]:   execution_index: [["b13f73ac8ced79cb093a638972923de1", 1], ["e654c4b77587b601e5a5767a82a27f45", 1]]
[FILIBUSTER] [INFO]: * Failed with metadata: [('return_value', {'status_code': '503'})]
[FILIBUSTER] [INFO]:
[FILIBUSTER] [INFO]:
[FILIBUSTER] [INFO]: Failures for this execution:
[FILIBUSTER] [INFO]: [["b13f73ac8ced79cb093a638972923de1", 1], ["e654c4b77587b601e5a5767a82a27f45", 1]]: [('return_value', {'status_code': '503'})]
[FILIBUSTER] [INFO]: =====================================================================================
127.0.0.1 - - [27/Sep/2021 10:35:05] "GET /filibuster/new-test-execution/foo HTTP/1.1" 200 -
127.0.0.1 - - [27/Sep/2021 10:35:05] "PUT /filibuster/create HTTP/1.1" 200 -
127.0.0.1 - - [27/Sep/2021 10:35:05] "POST /filibuster/update HTTP/1.1" 200 -
127.0.0.1 - - [27/Sep/2021 10:35:05] "GET /filibuster/new-test-execution/bar HTTP/1.1" 200 -
127.0.0.1 - - [27/Sep/2021 10:35:05] "PUT /filibuster/create HTTP/1.1" 200 -
127.0.0.1 - - [27/Sep/2021 10:35:05] "POST /filibuster/update HTTP/1.1" 200 -
127.0.0.1 - - [27/Sep/2021 10:35:05] "POST /filibuster/update HTTP/1.1" 200 -
Traceback (most recent call last):
    File "/private/tmp/filibuster-corpus/filibuster-tutorial/./functional/test_foo_bar_baz.py", line 19, in <module>
    test_functional_foo_bar_baz()
    File "/private/tmp/filibuster-corpus/filibuster-tutorial/./functional/test_foo_bar_baz.py", line 16, in test_functional_foo_bar_baz
    assert response.status_code == 200 and response.text == "foo bar baz"
AssertionError
[FILIBUSTER] [FAIL]: Test failed; counterexample file written: counterexample.json
```

What we see here is an assertion failure: the status code and text do not match when a fault was injected.  We can see
from further back in the output the precise fault that was injected.

```python
[FILIBUSTER] [INFO]: gen_id: 1
[FILIBUSTER] [INFO]:   module: requests
[FILIBUSTER] [INFO]:   method: get
[FILIBUSTER] [INFO]:   args: ['5002/baz']
[FILIBUSTER] [INFO]:   kwargs: {}
[FILIBUSTER] [INFO]:   vclock: {'foo': 1, 'bar': 1}
[FILIBUSTER] [INFO]:   origin_vclock: {'foo': 1}
[FILIBUSTER] [INFO]:   execution_index: [["b13f73ac8ced79cb093a638972923de1", 1], ["e654c4b77587b601e5a5767a82a27f45", 1]]
[FILIBUSTER] [INFO]: * Failed with metadata: [('return_value', {'status_code': '503'})]
```

Here, we see that the request from ``bar`` to ``baz`` was failed with a 503 Service Unavailable response.  This response caused the entire request to no longer return a 200 OK containing "foo bar baz".

If we want to re-run that precise test, we can using the counterexample that Filibuster provided.

```shell
filibuster --functional-test ./functional/test_foo_bar_baz.py --counterexample-file counterexample.json
```

Counterexample reproduced!

## Modify our Functional Test

In order to keep testing, we need to update our assertions in our test to reflect the behavior we expect under failure.

Instead of only ensuring that our three apps successfully return "foo bar baz" to a client, we also want to allow the
request to ``foo`` to fail gracefully.  To ensure the request fails only when it should, we should use the
``filibuster.assertions`` module. ``filibuster.assertions``'s ``was_fault_injected()`` tells us whether:

* a fault has been injected, meaning ``response.status_code`` should be a failure status code
* or not, meaning ``response.status_code`` should be ``200`` and "foo bar baz" should be returned

Adjust ``filibuster-tutorial/functional/test_foo_bar_baz.py`` to incorporate ``filibuster.assertions``'s ``was_fault_injected()`` so that it matches the following:

```python
#!/usr/bin/env python

import requests
import os
import sys

from filibuster.assertions import was_fault_injected

def test_functional_foo_bar_baz():
    response = requests.get("{}/foo/bar/baz".format("http://localhost:5000"), timeout=10)
    if response.status_code == 200:
        assert (not was_fault_injected()) and response.text == "foo bar baz"
    else:
        assert was_fault_injected() and response.status_code in [503, 404]

if __name__ == "__main__":
    test_functional_foo_bar_baz()
```

Filibuster's assertions module also provides a more granular assertion: ``was_fault_injected_on(service_name)`` that can
be used to write more precise assertions.

Let's re-run the counterexample; with our updated assertion, the test should now pass!

```shell
filibuster --functional-test ./functional/test_foo_bar_baz.py --counterexample-file counterexample.json
```

Now, we can run Filibuster again and test for the whole default set of failures as well.

```shell
filibuster --functional-test ./functional/test_foo_bar_baz.py
```

After 10 tests, we run into another failure.

```shell
[FILIBUSTER] [INFO]: Running test 11
[FILIBUSTER] [INFO]: Total tests pruned so far: 1
[FILIBUSTER] [INFO]: Total tests remaining: 0
[FILIBUSTER] [INFO]:
[FILIBUSTER] [INFO]: =====================================================================================
[FILIBUSTER] [INFO]: Test number: 11
[FILIBUSTER] [INFO]:
[FILIBUSTER] [INFO]: gen_id: 0
[FILIBUSTER] [INFO]:   module: requests
[FILIBUSTER] [INFO]:   method: get
[FILIBUSTER] [INFO]:   args: ['5001/bar/baz']
[FILIBUSTER] [INFO]:   kwargs: {}
[FILIBUSTER] [INFO]:   vclock: {'foo': 1}
[FILIBUSTER] [INFO]:   origin_vclock: {}
[FILIBUSTER] [INFO]:   execution_index: [["b13f73ac8ced79cb093a638972923de1", 1]]
[FILIBUSTER] [INFO]: * Failed with exception: {'name': 'requests.exceptions.ConnectionError', 'metadata': {}}
[FILIBUSTER] [INFO]:
[FILIBUSTER] [INFO]:
[FILIBUSTER] [INFO]: Failures for this execution:
[FILIBUSTER] [INFO]: [["b13f73ac8ced79cb093a638972923de1", 1]]: {'name': 'requests.exceptions.ConnectionError', 'metadata': {}}
[FILIBUSTER] [INFO]: =====================================================================================
127.0.0.1 - - [27/Sep/2021 10:55:54] "GET /filibuster/new-test-execution/foo HTTP/1.1" 200 -
127.0.0.1 - - [27/Sep/2021 10:55:54] "PUT /filibuster/create HTTP/1.1" 200 -
127.0.0.1 - - [27/Sep/2021 10:55:54] "POST /filibuster/update HTTP/1.1" 200 -
127.0.0.1 - - [27/Sep/2021 10:55:54] "GET /fault-injected HTTP/1.1" 200 -
Traceback (most recent call last):
    File "/private/tmp/filibuster-corpus/filibuster-tutorial/./functional/test_foo_bar_baz.py", line 24, in <module>
    test_functional_foo_bar_baz()
    File "/private/tmp/filibuster-corpus/filibuster-tutorial/./functional/test_foo_bar_baz.py", line 21, in test_functional_foo_bar_baz
    assert was_fault_injected() and response.status_code in [503, 404]
AssertionError
[FILIBUSTER] [FAIL]: Test failed; counterexample file written: counterexample.json
```

Again, we have another counterexample file.  If we look at the precise fault that was injected, we can see that the
request between ``foo`` and ``bar`` was failed with a ConnectionError exception.  Since the ``foo`` service does not
have an exception handler for this fault, the service returns a 500 Internal Server Error: we do not expect this response
in our functional test.

Instead of altering our functional test to allow for a 500 Internal Server Error, we want the service to return a 503
Service Unavailable if one of the dependencies is down.  Therefore, we will modify the implementation of the ``foo``
service to handle this failure.

```python
except requests.exceptions.ConnectionError:
    raise ServiceUnavailable("The bar service is unavailable.")
```

We can verify our fix using counterexample replay.

```shell
filibuster --functional-test ./functional/test_foo_bar_baz.py --counterexample-file counterexample.json
```

Finally, we can run Filibuster again and test for the whole default set of failures as well.

```shell
filibuster --functional-test ./functional/test_foo_bar_baz.py
```

At this point, everything passes!

## Computing Coverage

From here, you can use Filibuster to compute coverage.  Coverage files are not available until the services are shutdown,
so we must shut the services down.  Then, we can use the Filibuster tool to generate coverage, which will be rendered as
html in the ``htmlcov`` directory.

```shell
make local-stop
filibuster-coverage
```

You can see that, even though we only wrote a test that exercised the failure-free path of the ``foo`` service,
Filibuster automatically generated the necessary tests to cover the failure scenarios.  This coverage is aggregated
across all generated Filibuster tests and for all services.

<img src="/img/tutorial-coverage.png" width="600">

## Targeting Specific Faults

Up to now, we have been using Filibuster with a default set of faults.  However, what if your application generates
a failure that is not included in the default set?  To do that, we can use the Filibuster analysis tool to generate
a custom list of faults and failures to inject.

To do this, we run the following command.

```shell
filibuster-analysis --services-directory services --output-file analysis.json
```

This command will invoke the Filibuster static analysis tool.  The analysis tool will look in the directory ``services``
for the implementation of each service and output an ``analysis.json`` file that can be provided to Filibuster for
more targeted fault injection.

You should see output like the following:

```shell
[FILIBUSTER] [INFO]: About to analyze directory: services
[FILIBUSTER] [INFO]: * found service implementation: services/foo
[FILIBUSTER] [INFO]: * found service implementation: services/baz
[FILIBUSTER] [INFO]: * found service implementation: services/bar
[FILIBUSTER] [INFO]:
[FILIBUSTER] [INFO]: Found services: ['foo', 'baz', 'bar']
[FILIBUSTER] [INFO]:
[FILIBUSTER] [INFO]: Analyzing service foo at directory services/foo
[FILIBUSTER] [INFO]: * starting analysis of Python file: services/foo/foo/__init__.py
[FILIBUSTER] [INFO]: * identified HTTP error: {'return_value': {'status_code': '500'}}
[FILIBUSTER] [INFO]: * starting analysis of Python file: services/foo/foo/app.py
[FILIBUSTER] [INFO]: * identified HTTP error: {'return_value': {'status_code': '503'}}
[FILIBUSTER] [INFO]:
[FILIBUSTER] [INFO]: Analyzing service baz at directory services/baz
[FILIBUSTER] [INFO]: * starting analysis of Python file: services/baz/baz/__init__.py
[FILIBUSTER] [INFO]: * identified HTTP error: {'return_value': {'status_code': '500'}}
[FILIBUSTER] [INFO]: * starting analysis of Python file: services/baz/baz/app.py
[FILIBUSTER] [INFO]:
[FILIBUSTER] [INFO]: Analyzing service bar at directory services/bar
[FILIBUSTER] [INFO]: * starting analysis of Python file: services/bar/bar/__init__.py
[FILIBUSTER] [INFO]: * identified HTTP error: {'return_value': {'status_code': '500'}}
[FILIBUSTER] [INFO]: * starting analysis of Python file: services/bar/bar/app.py
[FILIBUSTER] [INFO]: * identified HTTP error: {'return_value': {'status_code': '503'}}
[FILIBUSTER] [INFO]:
[FILIBUSTER] [INFO]: Writing output file: analysis.json
[FILIBUSTER] [INFO]: Done.
```

From here, you can provide the analysis file directly to the Filibuster tool.

```shell
filibuster --functional-test ./functional/test_foo_bar_baz.py --analysis-file analysis.json
```

Nice!

## Conclusion

That was a short introduction to using Filibuster on an example application to find resilience bugs.  With our upcoming
release of Filibuster, we will release full documentation on our tool, an example corpus and this tutorial.

_In our next post, we'll look at algorithmic improvements we can use to reduce test case redundancy and make our system perform at scale.  Special thanks to Andrea Estrada for writing the first version of this tutorial._