

import Schema
import SimpleRecord
import Name
import HaskellCode

jess = Schema "jess" [(1,"Int"),(2,"Int"),(3,"Double"),(4,"String")] [(5,[1,2]),(6,[2,3,4])]
con = SubSchema "con" [Face 1 $ NamedSimplex 6] jess

sarah = SimpleRecordMetadata 
  {
  simpleRecordSchema = jess,
  simpleRecordCompressionSchemes = [(1,Lit "intCompress"),(2,Lit "intCompress2"),(3,Lit "david"),(4,Lit "sam")],
  simpleRecordDecompressionSchemes = [(1,Lit "deIntCompress"),(2,Lit "deIntCompress2"),(3,Lit "kim"),(4,Lit "ak")]
  }
  
