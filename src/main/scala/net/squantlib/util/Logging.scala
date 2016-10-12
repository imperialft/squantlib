package net.squantlib.util

import org.codehaus.jackson.JsonNode
import scala.language.implicitConversions
import scala.annotation.tailrec
import com.typesafe.scalalogging.slf4j._
import ch.qos.logback.classic.Logger
import ch.qos.logback.classic.encoder.PatternLayoutEncoder
import ch.qos.logback.core.FileAppender
import ch.qos.logback.core.util.StatusPrinter
import org.slf4j.LoggerFactory
import ch.qos.logback.classic.LoggerContext

object Logging {
  
//  var loggerContext: LoggerContext = (LoggerContext) LoggerFactory.getILoggerFactory()
//  var logbackLogger:Logger = loggerContext.getLogger("Main")
//  
//  def switchFile(name:String) {
//
//    FileAppender fileAppender = new FileAppender();
//    fileAppender.setContext(loggerContext);
//    fileAppender.setName("timestamp");
//    // set the file name
//    fileAppender.setFile("log/" + name + "-" + System.currentTimeMillis()+".log");
//
//    PatternLayoutEncoder encoder = new PatternLayoutEncoder();
//    encoder.setContext(loggerContext);
//    encoder.setPattern("%r %thread %level - %msg%n");
//    encoder.start();
//
//    fileAppender.setEncoder(encoder);
//    fileAppender.start();
//
//    // attach the rolling file appender to the logger of your choice
//    logbackLogger.addAppender(fileAppender);
//
//    // OPTIONAL: print logback internal status messages
//    StatusPrinter.print(loggerContext);
//
//    // log something
//    logbackLogger.debug("hello");
//  }
}
