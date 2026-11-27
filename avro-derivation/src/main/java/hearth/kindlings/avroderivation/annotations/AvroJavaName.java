package hearth.kindlings.avroderivation.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Overrides the name of the Avro ENUM derived for a Java enum (which cannot carry the Scala annotation
 * {@code @avroName}); the counterpart of avro4s' {@code @AvroJavaName}.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE})
public @interface AvroJavaName {
    String value();
}
