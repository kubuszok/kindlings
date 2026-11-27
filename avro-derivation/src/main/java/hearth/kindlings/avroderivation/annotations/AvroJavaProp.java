package hearth.kindlings.avroderivation.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Adds a custom property to the Avro ENUM derived for a Java enum (which cannot carry the Scala annotation
 * {@code @avroProp}); the counterpart of avro4s' {@code @AvroJavaProp}.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE})
public @interface AvroJavaProp {
    String key();

    String value();
}
