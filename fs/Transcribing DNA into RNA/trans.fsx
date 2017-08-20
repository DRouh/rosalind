let input = System.IO.File.ReadAllText("rosalind_rna.txt")
let transcribed = input.Replace("T", "U")
System.IO.File.WriteAllText("output.txt", transcribed)