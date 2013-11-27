
import Schema
import SimpleRecord
import SimpleSubInstance
import Name
import HaskellCode
import Metadata
import TupleUtils
import Shriek
import QueryCompile

jessSC = SC [1,2,3,4] [[1,2],[2,3,4]]
jess = Schema jessSC [(1,t_Int),(2,t_Int),(3,t_Double),(4,t_String)]
con = SubSchema [[2,3],[2,4]] jess
david = SubSchema [[1,2],[1],[2,4],[3,2,4]] jess
disjoint = SubSchema [[1],[2]] jess

samSC = SC [11,12,13,14] [[11],[12,13,14]]
sam = Schema samSC [(11,t_Int),(12,t_Int),(13,t_Double),(14,t_String)]
samJessMap = SchemaMap sam jess [(11,1,Lit "foo"),(12,2,Lit "foo"),(13,4,Lit "foo"),(14,3,Lit "foo")]

sarah = SimpleRecordMetadata 
  {
  simpleRecordSchema = jess,
  simpleRecordName = "sarah",
  simpleRecordCompressionSchemes = [(2,Lit "encodeLazy"),(3,Lit "encodeLazy"),(4,Lit "encodeLazy")],
  simpleRecordDecompressionSchemes = [(2,Lit "decodeLazy"),(3,Lit "decodeLazy"),(4,Lit "decodeLazy")]
  }
  
evan = SimpleSubInstanceMetadata 
  {
    simpleSubInstanceSimplex = [2,3],
    simpleSubInstanceName = "evan",
    simpleSubInstanceParamTypes = [t_Int],
    simpleSubInstanceFilterFunc = Lam (Tup [Ltp "x_1",Ltp "x_2"]) $ (Lit "x_1") +==+ (Lit "_param_1"),
    simpleSubInstanceInnerMetadata = sarah
  }

anna = ShriekMetadata 
  {
    shriekMap = samJessMap,
    shriekName = "anna",
    shriekInnerMetadata = evan
  }
  
