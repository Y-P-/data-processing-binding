package loader.annotations;

import java.lang.annotation.Target;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Inherited;

import loader.context.ClassContext;

@Inherited
@Target({ElementType.FIELD, ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
public @interface TagField {
  String inName()    default "";
  String outName()   default "=";
  Class<?> loader()  default ClassContext.Unknown.class;
  int min()          default 0;
  String check()     default "";
  String valid()     default "";
  String convert()   default "";
  String audit()     default "";
  String param()     default "";
}
