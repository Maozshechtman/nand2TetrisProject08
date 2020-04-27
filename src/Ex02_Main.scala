import java.io.{File, FileOutputStream, PrintWriter}

import EX01.Main_ex01.VMToASM

import scala.io.Source


object Ex02_Main {

  class VMtoASMConvertor(targetfile:File=null,sourcefile:String=null) extends VMToASM(targetFile = targetfile) {
    private final var returnAddresscount=0
    private final var  FunctionloopCounter=0
    private final val  ifGOTOASM=Array(
    "@SP",
    "M=M-1",
    "A=M",			//look for the head of the stack
    "D=M",
    "@FileName.c",
    "D;JNE"
    )
    private final val  pushReturnAddressASM=Array(
    "@g.ReturnAddress",
    "D=A",
    "@SP",
    "A=M",
    "M=D",
    "@SP",
    "M=M+1",

    )
    private final val saveRegistervaluesASM= Array(
     // save the values of registers LCL,ARG,THIS,THAT
    // push LCL
    "@LCL",
    "D=M",
    "@SP",
    "A=M",
    "M=D",
    "@SP",
    "M=M+1",
    //push ARG
      "@ARG",
      "D=M",
      "@SP",
      "A=M",
      "M=D",
      "@SP",
      "M=M+1",
     //push THIS
      "@THIS",
      "D=M",
      "@SP",
      "A=M",
      "M=D",
      "@SP",
      "M=M+1",
     //push THAT
      "@THAT",
      "D=M",
      "@SP",
      "A=M",
      "M=D",
      "@SP",
      "M=M+1",
    // ARG = SP-n-5
    "@SP",
    "D=M",
    "@newARG",  // = n-5,number
    "D=D-A",
    "@ARG",
   " M=D",
    // LCL = SP
    //In this action we mange the funcion local variables
    "@SP",
    "D=M",
    "@LCL",
    " M=D\n",
    // goto g (moves register A to the begin og fuction g)




    )
    private final val returnASM=Array(
    // FRAME = LCL
    "@LCL",
    "D=M",
    // RET = * (FRAME-5)
    // RAM[13] = (LOCAL - 5)
    "@5",
    "A=D-A",
    "D=M",
    "@13",
    "M=D",
    // * ARG = pop()
    "@SP",
    "M=M-1",
    "A=M",
    "D=M",
    "@ARG",
    "A=M",
    "M=D",
    // SP = ARG+1
    "@ARG",
    "D=M",
    "@SP",
    "M=D+1",

    // THAT = *(FRAM-1)
    "@LCL",
    "M=M-1",
    "A=M",
    "D=M",
   "@THAT",
    "M=D",

    // THIS = *(FRAM-2)
    "@LCL",
    "M=M-1",
    "A=M",
    "D=M",
    "@THIS",
    "M=D",
    // ARG = *(FRAM-3)
    "@LCL",
    "M=M-1",
    "A=M",
    "D=M",
    "@ARG",
    "M=D",
    // LCL = *(FRAM-4)
    "@LCL",
    "M=M-1",
    "A=M",
    "D=M",
    "@LCL",
    "M=D",
    // goto RET
    "@13",
    "A=M",
      "0;JMP\n"





    )
    writeBootTrapASM("Sys.init")
    override def parser(line: String): Unit = {
      val args=line.split(" ")
      args(0) match {
        case "label"=>writeLabelASM(args(1))
        case "if-goto"=>writeIfGoToASM(args(1))
        case "goto"=>writeGoToASM(args(1))
        case "call"=>writeCallASM(args(1),args(2))
        case "function"=> writeFunctionAsm(args(1),args(2))
        case "return"=>writeReturnASM()
        case _=>super.parser(line)
      }
    }

    private def writeLabelASM(label: String): Unit = {
      val writer = new PrintWriter(new FileOutputStream(targetfile, true))
      writer.append("("+sourcefile+"."+label+")"+"\n")
      writer.close()
    }
    private def writeGoToASM(label:String): Unit = {
      val writer = new PrintWriter(new FileOutputStream(targetfile, true))
      writer.append("@" + sourcefile + "." + label + "\n")
      writer.append("0;JMP" + "\n")
      writer.close()
    }
    private def writeIfGoToASM(label:String){
      val writer = new PrintWriter(new FileOutputStream(targetfile, true))
      for(cmd<-ifGOTOASM)
        {
          if(cmd.equals("@FileName.c"))
            writer.append("@"+sourcefile+"."+label+"\n")
          else
            writer.append(cmd+"\n")
        }
      writer.close()
    }
    private def writeCallASM(funcName:String,numbersOfParameters:String){
      val writer = new PrintWriter(new FileOutputStream(targetfile, true))
      for(cmd<-pushReturnAddressASM){
        val str=(if(cmd.equals("@g.ReturnAddress"))
          "@"+funcName + ".ReturnAddress"+returnAddresscount
        else cmd)+"\n"
        writer.append(str)
      }
      for (cmd<-saveRegistervaluesASM){
        if(cmd.equals("@newARG"))
          writer.append("@"+(numbersOfParameters.toInt+5+"\n"))
        else
          writer.append(cmd+"\n")
      }
      writer.append("@" + funcName + "\n")
      writer.append("0;JMP" + "\n")
      writer.append("("+funcName + ".ReturnAddress"+returnAddresscount+")"+"\n")
      writer.close()
      returnAddresscount+=1


    }
    private def writeFunctionAsm(name:String,numberOfLocals:String){
      val writer = new PrintWriter(new FileOutputStream(targetfile, true))
      writer.append("("+name+")\n")
      writer.append("@"+numberOfLocals+"\n")
      writer.append("D=A"+"\n")
      writer.append("@f.END"+FunctionloopCounter+"\n")
      writer.append("D;JEQ\n")
      writer.append("(f.Loop"+FunctionloopCounter+")\n")
      writer.append("@SP\n")
      writer.append("A=M\n M=0\n @SP\n M=M+1\n")
      writer.append("@f.Loop"+FunctionloopCounter+"\n"
      )
      writer.append("D=D-1;JNE\n")
      writer.append("(f.END"+FunctionloopCounter+")\n")
      writer.close()
      FunctionloopCounter+=1

  }
    private def writeReturnASM(): Unit ={
      val writer = new PrintWriter(new FileOutputStream(targetfile, true))
      writer.append(returnASM.mkString("\n"))
      writer.close()

    }
    private def writeBootTrapASM(funcName:String): Unit ={
      val writer = new PrintWriter(new FileOutputStream(targetfile, true))
      writer.append(
      "@256 \n"+		// sp=256
      "D=A \n"+
      "@SP\n"+
      "M=D\n")
      writer.close()
       writeCallASM(funcName,0.toString)

    }
}
  def deleteAsmFiles(dircetory: File): Unit = {
    for (file <- dircetory.listFiles()) {
      if (file.getName().endsWith(".asm"))
        file.delete()
    }
  }

  def readVMFiles(dircetory: File) {

    val files = dircetory.listFiles() //get files path
    val sourceFileName=dircetory.getName
    val fileName = dircetory.getPath() + "\\" +
      dircetory.getName()+
      ".asm"
    val asmConverter = new VMtoASMConvertor(new File(fileName),sourceFileName)

    for (file <- files) {
      if (file.getName().endsWith(".vm")) {
        println("Now I read from file:" + file.getName())
        //create new asm file


        for (line <- Source.fromFile(file).getLines()) {
          asmConverter.parser(line)


        }

      }
    }

  }


  def main(args: Array[String]): Unit = {

      println("Enter directory name:")
      val directory = scala.io.StdIn.readLine()
      //readVMFiles(path)

      deleteAsmFiles(new File(directory))
      readVMFiles(new File(directory))


  }
  }

