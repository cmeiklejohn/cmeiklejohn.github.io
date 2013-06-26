---
layout: post
title:  "Flume-ng configuration with an HDFS sink"
date:   2013-06-18 13:27:21 -0700
categories: flume hdfs
---

I've been playing around with [`flume-ng`][flume-ng] and its
[`HDFS`][hdfs] sink recently to try to understand how I can stream data
into `HDFS` and work with it using Hadoop.  The documentation for
`flume-ng` is unfortunately lacking, so I've typed up some quick notes
on how to configure and test the `HDFS` sink.

This document assumes that you have Hadoop installed and running
locally, with `flume-ng` version `1.2.0` or above.

In this example, the name of our agent is just `agent`.  First, let's
define a channel for `agent` named `memory-channel` of type `memory`.

{% highlight apache %}
# Define a memory channel on agent called memory-channel.
agent.channels.memory-channel.type = memory
{% endhighlight %}

Next, let's configure a source for `agent`, called `tail-source`, which
watches the `system.log` file.  Let us also assign it to the
`memory-channel`.

{% highlight apache %}
# Define a source on agent and connect to channel memory-channel.
agent.sources.tail-source.type = exec
agent.sources.tail-source.command = tail -F /var/log/system.log
agent.sources.tail-source.channels = memory-channel
{% endhighlight %}

Now, configure two sinks: logger and `HDFS`.  Then, we specify the path
to the name node for `HDFS`, pointing to the output path of where we
want the files stored.

{% highlight apache %}
# Define a sink that outputs to logger.
agent.sinks.log-sink.channel = memory-channel
agent.sinks.log-sink.type = logger

# Define a sink that outputs to hdfs.
agent.sinks.hdfs-sink.channel = memory-channel
agent.sinks.hdfs-sink.type = hdfs
agent.sinks.hdfs-sink.hdfs.path = hdfs://localhost:54310/tmp/system.log/
agent.sinks.hdfs-sink.hdfs.fileType = DataStream
{% endhighlight %}

Then, we configure the agent's channels, sources and sinks.

{% highlight apache %}
# Finally, activate.
agent.channels = memory-channel
agent.sources = tail-source
agent.sinks = log-sink hdfs-sink
{% endhighlight %}

Finally, let's start the flume agent, logging all output to the console,
and starting agent `agent`.

{% highlight bash %}
# Run flume-ng, with log messages to the console.
$ bin/flume-ng agent --conf ./conf/ -f conf/flume.conf \
    -Dflume.root.logger=DEBUG,console -n agent
{% endhighlight %}

Finally, you should see output like this as data is written to the
filesystem.

{% highlight bash %}
2013-06-18 14:00:49,784 (hdfs-hdfs-sink-call-runner-0) [INFO - org.apache.flume.sink.hdfs.BucketWriter.doOpen(BucketWriter.java:189)] Creating hdfs://localhost:54310/tmp/system.log//FlumeData.1371589249458.tmp
{% endhighlight %}

Success!

[flume-ng]: http://flume.apache.org/
[hdfs]: http://hadoop.apache.org/docs/stable/hdfs_design.html
