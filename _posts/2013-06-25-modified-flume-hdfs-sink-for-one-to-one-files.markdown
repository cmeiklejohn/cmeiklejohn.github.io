---
layout: post
title:  "Modified Flume HDFS sink for one to one files"
date:   2013-06-25 17:19:08 -0700
categories: hdfs flume
---

Today, I present a modified HDFS sink for [Flume][flume], purely a
prototype, with support for one-to-one file creation for each event.
This sink assumes that events will be ingested, or later
[intercepted][interceptor] to have a header associated with the event
with the destination filename.

First, we define a new configuration variable to determine that for a
particular HDFS sink, we want one event created per event.

{% highlight java %}
agent.sinks.hdfs-sink.hdfs.singleBucket = true
{% endhighlight %}

Now, let's modify the HDFS Event Sink that comes standard with [Flume][flume] to
use this configuration variable as a determination to perform this
alternate type of write.

{% highlight java %}
// Extract headers.
Map<String, String> headers = event.getHeaders();
String destinationName = headers.get("destinationName");

if(singleBucket && destinationName != null) {
  bucketWriter.appendSingle(event, destinationName);
} else {
  bucketWriter.append(event);
}
{% endhighlight %}

Now, let's implement the writer to create one file per event, based on
the header that's been appended to the event object.

{% highlight java %}
/**
 * Write a single object to HDFS.
 *
 * @throws IOException
 * @throws InterruptedException
 */
public synchronized void appendSingle(final Event event, final String destinationName)
  throws IOException, InterruptedException {
  if ((filePath == null) || (writer == null)) {
    throw new IOException("Invalid file settings");
  }

  final Configuration config = new Configuration();
  // disable FileSystem JVM shutdown hook
  config.setBoolean("fs.automatic.close", false);

  synchronized (staticLock) {
    checkAndThrowInterruptedException();

    if(destinationName != null) {
      try {
        bucketPath = filePath + "/" + destinationName + inUseSuffix;
        targetPath = filePath + "/" + destinationName;

        LOG.info("Creating " + bucketPath);

        callWithTimeout(new CallRunner<Void>() {
          @Override
          public Void call() throws Exception {
            // Open.
            if (codeC == null) {
              // Need to get reference to FS using above config before underlying
              // writer does in order to avoid shutdown hook & IllegalStateExceptions
              fileSystem = new Path(bucketPath).getFileSystem(config);
              writer.open(bucketPath);
            } else {
              // need to get reference to FS before writer does to avoid shutdown hook
              fileSystem = new Path(bucketPath).getFileSystem(config);
              writer.open(bucketPath, codeC, compType);
            }

            // Increment counters.
            sinkCounter.incrementConnectionCreatedCount();
            resetCounters();

            // Write.
            sinkCounter.incrementEventDrainAttemptCount();
            writer.append(event);
            writer.sync();

            // Close.
            writer.close();
            sinkCounter.incrementConnectionClosedCount();

            // Rename.
            renameBucket();

            return null;
          }
        });
      } catch (Exception ex) {
        sinkCounter.incrementConnectionFailedCount();
        if (ex instanceof IOException) {
          throw (IOException) ex;
        } else {
          throw Throwables.propagate(ex);
        }
      }
    }
  }
}
{% endhighlight %}

The code for this prototype is available on [GitHub][github].

[github]: https://github.com/cmeiklejohn/flume-ng
[flume]: http://flume.apache.org/
[interceptor]: http://flume.apache.org/releases/content/1.3.0/apidocs/org/apache/flume/interceptor/package-summary.html
