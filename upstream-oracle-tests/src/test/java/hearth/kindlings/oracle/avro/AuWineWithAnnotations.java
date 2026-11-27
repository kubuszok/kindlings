package hearth.kindlings.oracle.avro;

import com.sksamuel.avro4s.AvroJavaEnumDefault;
import com.sksamuel.avro4s.AvroJavaName;
import com.sksamuel.avro4s.AvroJavaNamespace;
import com.sksamuel.avro4s.AvroJavaProp;

// avro4s: schema/WineWithAnnotations.java (schema/EnumSchemaTest)
// (each library reads only its own annotations)
@AvroJavaName("Wine")
@hearth.kindlings.avroderivation.annotations.AvroJavaName("Wine")
@AvroJavaNamespace("test")
@hearth.kindlings.avroderivation.annotations.AvroJavaNamespace("test")
@AvroJavaProp(key = "hello", value = "world")
@hearth.kindlings.avroderivation.annotations.AvroJavaProp(key = "hello", value = "world")
public enum AuWineWithAnnotations {
    Malbec,
    Shiraz,
    @AvroJavaEnumDefault
    @hearth.kindlings.avroderivation.annotations.AvroJavaEnumDefault
    CabSav,
    Merlot
}
