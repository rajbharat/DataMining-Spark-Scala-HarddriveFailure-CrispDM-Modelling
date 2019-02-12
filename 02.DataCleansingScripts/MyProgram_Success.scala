package com.sundogsoftware.spark

import org.apache.spark._
import org.apache.spark.SparkContext._
import org.apache.spark.sql._
import org.apache.log4j._

object MyProgram {
    
    case class Person(date:String, serial_number:String, model:String, capacity_bytes:String, failure:String, smart_1_normalized:String, smart_1_raw:String, smart_2_normalized:String, smart_2_raw:String, smart_3_normalized:String, smart_3_raw:String, smart_4_normalized:String, smart_4_raw:String, smart_5_normalized:String, smart_5_raw:String, smart_7_normalized:String, smart_7_raw:String, smart_8_normalized:String, smart_8_raw:String, smart_9_normalized:String, smart_9_raw:String, smart_10_normalized:String, smart_10_raw:String, smart_11_normalized:String, smart_11_raw:String, smart_12_normalized:String, smart_12_raw:String, smart_13_normalized:String, smart_13_raw:String, smart_15_normalized:String, smart_15_raw:String, smart_22_normalized:String, smart_22_raw:String,  smart_177_normalized:String, smart_177_raw:String, smart_179_normalized:String, smart_179_raw:String, smart_181_normalized:String, smart_181_raw:String, smart_182_normalized:String, smart_182_raw:String, smart_183_normalized:String, smart_183_raw:String, smart_184_normalized:String, smart_184_raw:String, smart_187_normalized:String, smart_187_raw:String, smart_188_normalized:String, smart_188_raw:String, smart_189_normalized:String, smart_189_raw:String, smart_190_normalized:String, smart_190_raw:String, smart_191_normalized:String, smart_191_raw:String, smart_192_normalized:String, smart_192_raw:String, smart_193_normalized:String, smart_193_raw:String, smart_194_normalized:String, smart_194_raw:String, smart_195_normalized:String, smart_195_raw:String, smart_196_normalized:String, smart_196_raw:String, smart_197_normalized:String, smart_197_raw:String, smart_198_normalized:String, smart_198_raw:String, smart_199_normalized:String, smart_199_raw:String, smart_200_normalized:String, smart_200_raw:String, smart_201_normalized:String, smart_201_raw:String, smart_220_normalized:String, smart_220_raw:String, smart_222_normalized:String, smart_222_raw:String, smart_223_normalized:String, smart_223_raw:String, smart_224_normalized:String, smart_224_raw:String, smart_225_normalized:String, smart_225_raw:String, smart_226_normalized:String, smart_226_raw:String, smart_235_normalized:String, smart_235_raw:String, smart_240_normalized:String, smart_240_raw:String, smart_241_normalized:String, smart_241_raw:String, smart_242_normalized:String, smart_242_raw:String, smart_250_normalized:String, smart_250_raw:String, smart_251_normalized:String, smart_251_raw:String, smart_252_normalized:String, smart_252_raw:String, smart_254_normalized:String, smart_254_raw:String, smart_255_normalized:String, smart_255_raw:String)
    //case class Person(ID:String, name:String, age:String, numFriends:String)
  def mapper(line:String): Person = {
    val fields = line.split(",", -1)
    
    val person:Person = Person(fields(0), fields(1), fields(2), fields(3), fields(4), fields(5), fields(6), fields(7), fields(8), fields(9), fields(10), fields(11), fields(12), fields(13), fields(14), fields(15), fields(16), fields(17), fields(18), fields(19), fields(20), fields(21), fields(22), fields(23), fields(24), fields(25), fields(26), fields(27), fields(28), fields(29), fields(30), fields(31), fields(32), fields(33), fields(34), fields(35), fields(36), fields(37), fields(38), fields(39), fields(40), fields(41), fields(42), fields(43), fields(44), fields(45), fields(46), fields(47), fields(48), fields(49), fields(50), fields(51), fields(52), fields(53), fields(54), fields(55), fields(56), fields(57), fields(58), fields(59), fields(60), fields(61), fields(62), fields(63), fields(64), fields(65), fields(66), fields(67), fields(68), fields(69), fields(70), fields(71), fields(72), fields(73), fields(74), fields(75), fields(76), fields(77), fields(78), fields(79), fields(80), fields(81), fields(82), fields(83), fields(84), fields(85), fields(86), fields(87), fields(88), fields(89), fields(90), fields(91), fields(92), fields(93), fields(94), fields(95), fields(96), fields(97), fields(98), fields(99), fields(100), fields(101), fields(102), fields(103), fields(104))
    // val person:Person = Person(fields(0), fields(1), fields(2), fields(3))

    return person
  }
  
  /** Our main function where the action happens */
  def main(args: Array[String]) {
    
    // Set the log level to only print errors
    Logger.getLogger("org").setLevel(Level.ERROR)
    
    // Use new SparkSession interface in Spark 2.0
    val spark = SparkSession
      .builder
      .appName("SparkSQL")
      .master("local[*]")
      .config("spark.sql.warehouse.dir", "file:///C:/temp") // Necessary to work around a Windows bug in Spark 2.0.0; omit if you're not on Windows.
      .getOrCreate()
    import spark.implicits._
    
    val lines = spark.sparkContext.textFile("file:///C:/data/data_2018/*.csv")

    val people = lines.map(mapper).toDS().cache()

    // Infer the schema, and register the DataSet as a table.

     System.out.println("converted to data set")
    people.printSchema()
    
    people.createOrReplaceTempView("peopleTemp")
     System.out.println("created a dataset with temptable")
     System.out.println("Failure started")
    // SQL can be run over DataFrames that have been registered as a table
    val failures = spark.sql("SELECT * FROM peopleTemp where failure=1 and model='ST4000DM000'")
    //people.filter(people("failure") == 1).show()
    //val results = teenagers.collect()
    
    //failures.write.option("header", "true").csv("file:///C:/data/fail.csv")
    failures.coalesce(1).write.format("com.databricks.spark.csv").option("header","true").save("file:///C:/data/HD_fail_2018")
    
    System.out.println("Failure done")
   
    System.out.println("Success started")
     val success = spark.sql("select * from (SELECT *, dense_rank() OVER (PARTITION BY serial_number ORDER BY date DESC) as LatestRec  FROM peopleTemp ) where failure=0 and model='ST4000DM000' and CAST(smart_9_raw as INT) > 8760 and LatestRec=1")

     success.coalesce(1).write.format("com.databricks.spark.csv").option("header","true").save("file:///C:/data/HD_success_2018")
    
    
    System.out.println("Success done")
    spark.stop()
  }
}
