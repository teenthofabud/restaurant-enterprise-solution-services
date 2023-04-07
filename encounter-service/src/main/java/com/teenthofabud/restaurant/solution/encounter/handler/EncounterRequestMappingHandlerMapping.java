package com.teenthofabud.restaurant.solution.encounter.handler;

import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.mvc.condition.PatternsRequestCondition;
import org.springframework.web.servlet.mvc.method.RequestMappingInfo;
import org.springframework.web.servlet.mvc.method.annotation.RequestMappingHandlerMapping;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

public class EncounterRequestMappingHandlerMapping extends RequestMappingHandlerMapping {

    @Override
    protected RequestMappingInfo getMappingForMethod(Method method, Class<?> handlerType) {
        RequestMappingInfo methodMapping = super.getMappingForMethod(method, handlerType);
        if (methodMapping == null)
            return null;
        List<String> superclassUrlPatterns = new ArrayList<String>();
        boolean springPath = false;
        for (Class<?> clazz = handlerType; clazz != Object.class; clazz = clazz.getSuperclass())
            if (clazz.isAnnotationPresent(RequestMapping.class))
                if (springPath)
                    superclassUrlPatterns.add(clazz.getAnnotation(RequestMapping.class).value()[0]);// TODO handle other elements in the array if necessary
                else
                    springPath = true;
        if (!superclassUrlPatterns.isEmpty()) {
            RequestMappingInfo superclassRequestMappingInfo = new RequestMappingInfo("",
                    new PatternsRequestCondition(String.join("", superclassUrlPatterns)), null, null, null, null, null, null);// TODO implement specific method, consumes, produces, etc depending on your merging policies
            return superclassRequestMappingInfo.combine(methodMapping);
        } else
            return methodMapping;
    }

}
