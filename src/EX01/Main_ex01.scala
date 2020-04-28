package EX01

import java.io.{File, FileOutputStream, PrintWriter}

import scala.io.Source

object Main_ex01 {

  class VMToASM(targetFile: File = null,private var sourcefileName:String=null) {
    //setter for source file name of static
    def setSourcefileName(name:String): Unit ={
      sourcefileName=name
    }
    private var coditionCount = 0
    //<editor-fold desc="Operators">

    private final val addASM: Array[String] =
      Array(
        "@SP",
        "A=M-1",
        "D=M",
        "A=A-1",
        "M=D+M",
        "@SP",
        "M=M-1"

      )
    private final val subASM =
      Array(
        "@SP", //sp=256
        "A=M-1", //255
        "D=M", //D=ram[255]
        "A=A-1", //254
        "M=M-D", //ram[254]=ram[254]-ram[255]
        "@SP", //
        "M=M-1"

      )
    private final val orASM =
      Array("@SP",
        "A=M-1",
        "D=M",
        "A=A-1",
        "M=M|D",
        "@SP",
        "M=M-1")
    private final val andASM =
      Array("@SP",
        "@SP",
        "A=M-1",
        "D=M",
        "A=A-1",
        "M=D&M",
        "@SP",
        "M=M-1")
    private final val notASM =
      Array("@SP",
        "A=M-1",
        "D=M",
        "M=!D")
    private final val negASM =
      Array("@SP",
        "A=M-1",
        "D=M",
        "M=-D"


      )

    //</editor-fold>


    //<editor-fold desc="Logic Operators">

    private final val eqASM: Array[String] =
      Array(
        "@SP",
        "A=M-1",
        "D=M",
        "A=A-1",
        "D=D-M",
        "@IF_TRUE",
        "D;JEQ",
        "D=0",
        "@SP",
        "A=M-1",
        "A=A-1",
        "M=D",
        "@IF_FALSE",
        "0;JMP",
        "(IF_TRUE)", //label
        "D=-1",
        "@SP",
        "A=M-1",
        "A=A-1",
        "M=D",
        "(IF_FALSE)",
        "@SP",
        " M=M-1"
      )
    private final val gtASM =
      Array(
        "@SP",
        "A=M-1",
        "D=M",
        "A=A-1",
        "D=M-D",
        "@IF_TRUE",
        "D;JGT",
        "D=0",
        "@SP",
        "A=M-1",
        "A=A-1",
        "M=D",
        "@IF_FALSE",
        "0;JMP",
        "(IF_TRUE)", //label
        "D=-1",
        "@SP",
        "A=M-1",
        "A=A-1",
        "M=D",
        "(IF_FALSE)",
        "@SP",
        " M=M-1"
      )
    private final val ltASM =
      Array(
        "@SP",
        "A=M-1",
        "D=M",
        "A=A-1",
        "D=M-D",
        "@IF_TRUE",
        "D;JLT",
        "D=0",
        "@SP",
        "A=M-1",
        "A=A-1",
        "M=D",
        "@IF_FALSE",
        "0;JMP",
        "(IF_TRUE)", //label
        "D=-1",
        "@SP",
        "A=M-1",
        "A=A-1",
        "M=D",
        "(IF_FALSE)",
        "@SP",
        " M=M-1"
      )

    //</editor-fold>

    //<editor-fold desc="Memory access">

    private final val pushConstASM = Array(
      "@",
      "D=A",
      "@SP",
      "A=M",
      "M=D",
      "@SP",
      "M=M+1"
    )
    //note :There is no pop action for const in our compiler
    private final val pushStaticASM = Array("@ClassA.0", // class name.variable
      "D=M",
      "@SP",
      "A=M",
      "M=D",
      "@SP",
      "M=M+1"
    )
    private final val popStaticASM = Array("@SP",
      "M=M-1",
      "A=M",
      "D=M",
      "@ClassA.0", // @16 class name.variable
      "M=D"
    )
    private final val pushThisASM = Array(
      "@",
      "D=A",
      "@THIS",
      "A=M+D"
      ,"D=M",
      "@SP",
      "A=M",
      "M=D",
        "@SP",
      "M=M+1")
    private final val popThisASM = Array(
      "@",
      "D=A",
      "@THIS",
      "D=M+D",
      "@R13",
      "M=D",
      "@SP",
      "M=M-1",
      "A=M",
      "D=M",
      "@13",
      "A=M",
      "M=D"
    )
    private final val pushThatASM = Array(
      "@",
      "D=A",
      "@THAT",
      "A=M+D"
      ,"D=M",
      "@SP",
      "A=M",
      "M=D",
      "@SP",
      "M=M+1"
    )
    private final val popThatASM = Array(
      "@",
      "D=A",
      "@THAT",
      "D=M+D",
      "@R13",
      "M=D",
      "@SP",
      "M=M-1",
      "A=M",
      "D=M",
      "@13",
      "A=M",
      "M=D"
    )
    private final val pushLocalASM = Array("@", "D=A", "@LCL", "A=M+D", "D=M", "@SP", "A=M",
      "M=D", "@SP", "M=M+1")
    private final val popLocalASM = Array(
      "@",
      "D=A",
      "@LCL",
      "D=M+D",
      "@R13",
      "M=D",
      "@SP",
      "M=M-1",
      "A=M",
      "D=M",
      "@13",
      "A=M",
      "M=D"

    )
    private final val pushArgumentASM = Array("@", "D=A", "@ARG", "A=M+D", "D=M", "@SP", "A=M",
      "M=D", "@SP", "M=M+1")
    private final val popArgumentASM = Array(
      "@",
      "D=A",
      "@ARG",
      "D=M+D",
      "@R13",
      "M=D",
      "@SP",
      "M=M-1",
      "A=M",
      "D=M",
      "@13",
      "A=M",
      "M=D"
    )
    private final val pushTempASM = Array("@", // 5 + i done in compiler
      "D=M",
      "@SP",
      "A=M",
      "M=D",
      "@SP",
      "M=M+1")
    private final val popTempASM = Array(
      "@SP",
      "A=M-1",
      "D=M",
      "@" //5+var
      ,"M=D",
      "@SP",
      "M=M-1")
    private final val pushPointer0ASM = Array("@THIS",
      "D=M", "@SP", "A=M", "M=D", "@SP", "M=M+1")
    private final val popPointer0ASM = Array("@SP", "A=M-1", "D=M", "@THIS"
      , "M=D", "@SP", "M=M-1")
    private final val pushPointer1ASM = Array("@THAT",
      "D=M", "@SP", "A=M", "M=D", "@SP", "M=M+1")
    private final val popPointer1ASM = Array("@SP", "A=M-1", "D=M", "@THAT"
      , "M=D", "@SP", "M=M-1")

    //</editor-fold>


    def parser(line: String) {
      if (line.startsWith("//") || line.startsWith("\\"))
        return
      var args = line.split(" ")
      args(0) match {
        //Operators
        case "add" => writeASMOperator(targetFile, addASM)
        case "sub" => writeASMOperator(targetFile, subASM)
        case "or" => writeASMOperator(targetFile, orASM)
        case "and" => writeASMOperator(targetFile, andASM)
        //Unary operators
        case "neg" => writeASMOperator(targetFile, negASM)
        case "not" => writeASMOperator(targetFile, notASM)
        //Logic (if/else)
        case "eq" => writeASMLogicOperator(targetFile, eqASM)
        case "gt" => writeASMLogicOperator(targetFile, gtASM)
        case "lt" => writeASMLogicOperator(targetFile, ltASM)
        //Memory access
        case "push" => pushCase(args)
        case "pop" => popCase(args(1),args(2))
        case whoa => return "Compilation Error token " + args(0) + " is not legal!!"
      }


    }


    private def pushCase(args: Array[String]): Unit = {
      args(1) match {
        case "constant" => writepushConst(targetFile, args(2))
        case "local" => writePushLocal(targetFile, args(2))
        case "this" => writePushThis(targetFile, args(2))
        case "that" => writePushThat(targetFile, args(2))
        case "pointer" => if (args(2).equals("0") )
                            writePushPointer(pushPointer0ASM, targetFile)
                            else
                              writePushPointer(pushPointer1ASM, targetFile)
        case "static" => writePushStatic(targetFile, args(2),sourcefileName)
        case "argument" => writePushArgument(targetFile, args(2))
        case "temp" => writePushTemp(targetFile: File, args(2))
        case whoa => return "Compilation error.tokken " + args(1).toString() + "is not leagal!!"
      }

    }

    private def popCase(segmentType:String,arg: String): Unit = {
       segmentType match {
        case "local" => writePopLocal(targetFile,arg)
        case "this" => writePopThis(targetFile,arg)
        case "that" => writePopThat(targetFile,arg)
        case "pointer" => if(arg.equals("0"))
                              writePopPointer(popPointer0ASM, targetFile)
                           else
                              writePopPointer(popPointer1ASM, targetFile)
        case "static" => writePopStatic(targetFile, arg,sourcefileName)
        case "argument" => writePopArgument(targetFile,arg)
        case "temp" => writePopTemp(targetFile, arg)
        case whoa => return "Compilation error.tokken " + arg.toString() + "is not leagal!!"
      }

    }

    private def writeASMOperator(file: File, commndASM: Array[String]): Unit = {
      val writer = new PrintWriter(new FileOutputStream(file, true))
      for (cmd <- commndASM)
        writer.append(cmd + "\n")
      writer.close()
    }

    def writeASMLogicOperator(file: File, logicASMCmd: Array[String]): Unit = {
      val writer = new PrintWriter(new FileOutputStream(file, true))

      for (cmd <- logicASMCmd) {
        if (cmd.contains("TRUE") || cmd.contains("FALSE")) {
          if (cmd.contains(")"))
            writer.append(cmd.replace(")", coditionCount.toString() + ")") + "\n")
          else writer.append(cmd + coditionCount.toString() + "\n")
        }
        else
          writer.append(cmd + "\n")


      }
      writer.close()
      coditionCount = coditionCount + 1

    }

    private def writepushConst(file: File, arg: String): Unit = {
      val x = pushConstASM(0) //keep the value of the first element for initialize
      pushConstASM(0) = pushConstASM(0) + arg
      val writer = new PrintWriter(new FileOutputStream(file, true))
      for (cmd <- pushConstASM) {
        writer.append(cmd + "\n")
      }
      writer.close()
      pushConstASM(0) = x
    }

    private def writePushLocal(file: File, value: String): Unit = {
      val x = pushLocalASM(0) //keep the value of the first element for initialize
      pushLocalASM(0) = pushLocalASM(0) + value
      val writer = new PrintWriter(new FileOutputStream(file, true))
      for (cmd <- pushLocalASM) {
        writer.append(cmd + "\n")
      }
      writer.close()
      pushLocalASM(0) = x
    }
    private def writePopLocal(file: File, value: String): Unit = {

      val writer = new PrintWriter(new FileOutputStream(file, true))
      for (cmd <- popLocalASM) {
        if(cmd.equals("@"))
          writer.write("@"+value+"\n")
        else
        writer.append(cmd + "\n")
      }
      writer.close()

    }
    private def writePushArgument(file: File, value: String): Unit = {
      val writer = new PrintWriter(new FileOutputStream(file, true))
      for (cmd <- pushArgumentASM) {
        if (cmd.equals("@"))
          writer.append("@" + value.toString() + "\n")
        else
          writer.append(cmd + "\n")

      }
      writer.close()
    }
    private def writePopArgument(file: File, value: String): Unit = {

      val writer = new PrintWriter(new FileOutputStream(file, true))
      for (cmd <- popArgumentASM) {
        if(cmd.equals("@"))
          writer.write("@"+value+"\n")
        else
          writer.append(cmd + "\n")
      }
      writer.close()

    }
    private def writePushPointer(ASMcmds: Array[String], file: File): Unit = {
      val writer = new PrintWriter(new FileOutputStream(file, true))
      for (cmd <- ASMcmds) {
        writer.append(cmd + "\n")
      }
      writer.close()
    }
    private def writePopPointer(ASMcmds: Array[String], file: File): Unit = {
      val writer = new PrintWriter(new FileOutputStream(file, true))
      for (cmd <- ASMcmds) {
        writer.append(cmd + "\n")
      }
      writer.close()
    }
    private def writePushThat(file: File, value: String) {
      val writer = new PrintWriter(new FileOutputStream(file, true))
      for (cmd <- pushThatASM) {
        if (cmd.equals("@"))
          writer.append("@" + value.toString() + "\n")
        else
          writer.append(cmd + "\n")

      }
      writer.close()
    }
    private def writePopThat(file: File, value: String): Unit = {

      val writer = new PrintWriter(new FileOutputStream(file, true))
      for (cmd <- popThatASM) {
        if(cmd.equals("@"))
          writer.write("@"+value+"\n")
        else
          writer.append(cmd + "\n")
      }
      writer.close()

    }
    private def writePushThis(file: File, value: String) {
      val writer = new PrintWriter(new FileOutputStream(file, true))
      for (cmd <- pushThisASM) {
        if (cmd.equals("@"))
          writer.append("@" + value.toString() + "\n")
        else
          writer.append(cmd + "\n")

      }
      writer.close()
    }
    private def writePopThis(file:File,value:String) {
      val writer = new PrintWriter(new FileOutputStream(file, true))
      for(cmd<-popThisASM)
        {
          if(cmd.equals("@"))
            {

              writer.append("@"+value+"\n")
            }
          else
          writer.append(cmd+"\n")
        }
      writer.close()
    }
    private def writePushStatic(file: File, value: String,className:String): Unit = {
      val writer = new PrintWriter(new FileOutputStream(file, true))
      for (cmd <- pushStaticASM) {
        if (cmd.equals("@ClassA.0"))
          writer.append("@"+className+"." + value.toString() + "\n")
        else
          writer.append(cmd + "\n")
      }
      writer.close()
    }

    private def writePopStatic(file: File, value: String, className:String): Unit = {
      val writer = new PrintWriter(new FileOutputStream(file, true))
      for (cmd <- popStaticASM) {
        if (cmd.equals("@ClassA.0"))
          writer.append("@" + className+"."+value.toString() + "\n")
        else
          writer.append(cmd + "\n")
      }
      writer.close()
    }

    private def writePopTemp(file: File, value: String) {
      val writer = new PrintWriter(new FileOutputStream(file, true))
      for (cmd <- popTempASM) {
        if (cmd.equals("@"))
          writer.append(cmd + (value.toInt + 5) + "\n")
        else
          writer.append(cmd + "\n")
      }
      writer.close()

    }

    private def writePushTemp(file: File, value: String) {
      val writer = new PrintWriter(new FileOutputStream(file, true))
      for (cmd <- pushTempASM) {
        if (cmd.equals("@"))
          writer.append(cmd + (value.toInt + 5) + "\n")
        else
          writer.append(cmd + "\n")

      }
      writer.close()

    }

  }

  private def deleteAsmFiles(dircetory: File): Unit =
  {
    for(file<-dircetory.listFiles())
      {
        if(file.getName().endsWith(".asm"))
          file.delete()
      }
  }
  private def readVMFiles(dircetory: File) {

    val files = dircetory.listFiles() //get files path
    for (file <- files) {
      if (file.getName().endsWith(".vm")) {
        println("Now I read from file:" + file.getName())
        //create new asm file
        val fileName = dircetory.getPath() + "\\" +
          file.getName().replaceAll(".vm", "") +
          ".asm"
        val asmConverter = new VMToASM(new File(fileName))
        for (line <- Source.fromFile(file).getLines()) {
          asmConverter.parser(line)


        }

      }
    }
  }


  def main(args: Array[String]): Unit = {
    try {
      println("Enter directory name:")
      val directory = scala.io.StdIn.readLine()
      //readVMFiles(path)
      deleteAsmFiles(new File(directory))
      readVMFiles(new File(directory))
    }
    catch {
      case _:Throwable => println("genral error!!")
    }
  }
}
