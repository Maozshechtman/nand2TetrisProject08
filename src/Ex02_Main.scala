import java.io.{File, FileOutputStream, PrintWriter}

import EX01.Main_ex01.VMToASM

import scala.io.Source


object Ex02_Main {

  class VMtoASMConvertor(targetfile:File=null,sourcefile:String=null) extends VMToASM(targetFile = targetfile) {

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
    " M=D",
    // goto g (moves register A to the begin og fuction g)




    )
    override def parser(line: String): Unit = {
      val args=line.split(" ")
      args(0) match {
        case "label"=>writeLabelASM(args(1))
        case "if-goto"=>writeIfGoToASM(args(1))
        case "goto"=>writeGoToASM(args(1))
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
    private def wrirteCallASM(label:String){
      val writer = new PrintWriter(new FileOutputStream(targetfile, true))
      for(cmd<-pushReturnAddressASM){
        val str=(if(cmd.equals("@g.ReturnAddress"))
          "@"+sourcefile + "."+label
        else cmd)+"\n"
        writer.append(str)
      }
      writer.append(saveRegistervaluesASM.mkString("\n"))
      writer.close()
      writeGoToASM(label)

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
    for (file <- files) {
      if (file.getName().endsWith(".vm")) {
        println("Now I read from file:" + file.getName())
        //create new asm file
        val sourceFileName=file.getName
        val fileName = dircetory.getPath() + "\\" +
          file.getName().replaceAll(".vm", "") +
          ".asm"
        val asmConverter = new VMtoASMConvertor(new File(fileName),sourceFileName)

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
