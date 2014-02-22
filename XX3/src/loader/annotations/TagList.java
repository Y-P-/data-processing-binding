package loader.annotations;

import java.lang.annotation.Target;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Inherited;

import loader.context.ClassContext;
import loader.core.names.QName;

@Inherited
@Target({ElementType.FIELD, ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
public @interface TagList {
  String inName()   default "";
  String outName()  default "=";
  Class<? extends QName.Builder> qName() default QName.NoProc.class;
  Class<?> loader() default ClassContext.Unknown.class;
  int min()         default 0;
  int max()         default 0;
  String check()    default "";
  String valid()    default "";
  String param()    default "";
  String convert()  default "";
  String audit()    default "";
}
