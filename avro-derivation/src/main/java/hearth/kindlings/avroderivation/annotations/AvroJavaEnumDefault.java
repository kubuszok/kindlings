package hearth.kindlings.avroderivation.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Marks the constant of a Java enum that is the default symbol of the derived Avro ENUM (a Java enum cannot carry the
 * Scala annotation {@code @avroEnumDefault}); the counterpart of avro4s' {@code @AvroJavaEnumDefault}.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD})
public @interface AvroJavaEnumDefault {}
