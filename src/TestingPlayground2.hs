
import Schema
import SimpleRecord
import SimpleSubInstance
import Name
import HaskellCode
import Metadata
import TupleUtils
import Shriek
import InverseImage

milesSC = SC [1,2] [[1,2]]
miles = Schema milesSC [(1,t_Int),(2,t_Int)]
davisSC = SC [3] [[3]]
davis = Schema davisSC [(3,t_Int)]

ron = SubSchema [[2],[2,1]] miles

milesDavis = SchemaMap miles davis [(1,3,Lam (Ltp "x") $ c_2 "foo" (Lit "_param_1") (Lit "x")),(2,3,Lit "bar")]

wayne = SimpleRecordMetadata
  {
  simpleRecordSchema = davis,
  simpleRecordName = "wayne",
  simpleRecordCompressionSchemes = [(3,Lit "esp_compress")],
  simpleRecordDecompressionSchemes = [(3,Lit "footprint_decompress")]
  }
  
tony = InverseImageMetadata 
  {
    inverseImageMap = milesDavis,
    inverseImageName = "tony",
    inverseImageInnerMetadata = wayne,
    inverseImageParamTypes = [t_Int,t_Double]
  }

