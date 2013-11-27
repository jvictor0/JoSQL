
import Schema
import SimpleRecord
import SimpleSubInstance
import Name
import HaskellCode
import Metadata
import TupleUtils
import Shriek
import QueryCompile
import Execute
import NutleyInstance
import Include

jessSC = SC [1,2,3,4] [[1,2],[2,3,4]]
jess = Schema jessSC [(1,t_Int),(2,t_Int),(3,t_Double),(4,t_String)]
con = SubSchema [[2,3],[2,4]] jess
david = SubSchema [[1,2],[1],[2,4],[3,2,4]] jess
disjoint = SubSchema [[3],[2]] jess

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
  
doit = do
  clearAll
  let dt = [(Just 1,Just 1.0,Just "one"),
            (Just 2,Just 2.0,Just "two"),
            (Just 2,Just 2.5,Just "two point 5"),
            (Just 3,Just 2.5, Nothing)]
  rec <- executeInstantiate (undefined :: SimpleRecord) (InstantiateQuery sarah) 1 dt
  (l1,l2) <- executeSection (undefined::([(Int,Double)],[(Int,String)])) (MaterializeQuery sarah con) rec
  sec <- executeSection ([]::[(Int,Double,String)]) (SectionQuery sarah con) rec
  sec2 <- executeSection ([]::[(Int,Double,String)]) (SectionQuery evan con) $ SimpleSubInstance (2::Int) rec
  sec3 <- executeSection (undefined::[(Double,Int)]) (SectionQuery evan disjoint) $ SimpleSubInstance (2::Int) rec
  putStrLn "Materialize (2,3):"
  mapM_ print l1
  putStrLn "Materialize (2,4):"
  mapM_ print l2
  putStrLn "Section:"
  mapM_ print sec
  putStrLn "Evan Section:"
  mapM_ print sec2
  putStrLn "Evan Section2:"
  mapM_ print $ sec3



