package loader.annotations;

import java.lang.annotation.Target;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Inherited;

@Inherited
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
public @interface TagStruct {
	int auditMin() default 0;  //minimal audit for the class
	int auditMax() default 5;  //maximal audit for the class
	boolean fast() default true;
}
