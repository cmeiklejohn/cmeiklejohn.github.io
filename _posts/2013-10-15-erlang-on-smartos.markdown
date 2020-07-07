---
layout: post
title:  "Erlang/OTP R16B02 on SmartOS"
date:   2013-10-15 11:26:35 -0400
categories: ruby smartos
---

Mainly, so I don't forget, here's a series of configure flags that allow
you to build Erlang R16B02 on SmartOS successfully.  This was done on a
Joyent SmartMachine.

{% highlight sh %}
./configure --enable-hipe --enable-smp-support --enable-threads \
            --enable-kernel-poll --with-ssl=/opt/local \
            --without-javac --enable-m64-build --without-odbc \
            --enable-native-libs
{% endhighlight %}

There are a few interesting things here:

* This enables HiPE.  Supposedly there are problems with HiPE on any of
  the OpenSolaris derived systems, specifically OmniOS and SmartOS.  I
  build this release in the process of testing HiPE, so I've enabled
  both the HiPE application and native library compilation.
* I've disabled the JInterface application, because I
  was running into problems with the generated Makefile.  See below.
* I've also disabled the ODBC application, because the
  freetds package in SmartOS, while providing the correct header files
  appears to be missing some of the runtime library dependencies needed.
* Finally, I've manually specified the path to the correct OpenSSL
  package, which is located in /opt/local so the crypto application
  builds correctly.

# JInterface Makefile Problems

I've submitted a bug to Erlang/OTP, but the Makefile generation for
JInterface also has some problems.  Here's an excerpt of my post to
erlang-bugs:

{% highlight sh %}
There are a few places where the generated Makefiles for the interface
integration fail compilation because they assume that /bin/sh supports
the '-d' flag, which it does not on SmartOS.  I believe the solution
here is to adapt the Makefile so that this command is changed to:

if [ ! test -d "/root/repos/otp_src_R16B02/lib/jinterface/priv/" ]

=== Entering application jinterface
make[3]: Entering directory `/root/repos/otp_src_R16B02/lib/jinterface/java_src'
make[4]: Entering directory `/root/repos/otp_src_R16B02/lib/jinterface/java_src/com/ericsson/otp/erlang'
if [ ! -d "/root/repos/otp_src_R16B02/lib/jinterface/priv/" ];then mkdir "/root/repos/otp_src_R16B02/lib/jinterface/priv/"; fi
CLASSPATH=/root/repos/otp_src_R16B02/lib/jinterface/java_src/ -d /root/repos/otp_src_R16B02/lib/jinterface/priv/ OtpAuthException.java
/bin/sh: line 1: -d: not found
make[4]: *** [/root/repos/otp_src_R16B02/lib/jinterface/priv/com/ericsson/otp/erlang/OtpAuthException.class] Error 127
make[4]: Leaving directory `/root/repos/otp_src_R16B02/lib/jinterface/java_src/com/ericsson/otp/erlang'
make[3]: *** [opt] Error 2
make[3]: Leaving directory `/root/repos/otp_src_R16B02/lib/jinterface/java_src'
make[2]: *** [opt] Error 2
make[2]: Leaving directory `/root/repos/otp_src_R16B02/lib/jinterface'
make[1]: *** [opt] Error 2
make[1]: Leaving directory `/root/repos/otp_src_R16B02/lib'
make: *** [tertiary_bootstrap_build] Error 2

This is running on the following build of SmartOS: SunOS 5.11 joyent_20130322T181205Z i86pc i386 i86pc Solaris
{% endhighlight %}

Hopefully this helps next time you need to build Erlang on SmartOS.
