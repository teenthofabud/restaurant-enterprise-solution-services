package com.teenthofabud.restaurant.solution.customer.integration.metadata.gender.validator;

import com.teenthofabud.restaurant.solution.customer.error.CustomerErrorCode;
import com.teenthofabud.restaurant.solution.customer.integration.metadata.gender.data.GenderVo;
import com.teenthofabud.restaurant.solution.customer.integration.metadata.gender.proxy.GenderServiceClient;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

@Component
@Slf4j
public class GenderIdValidator implements Validator {

    private GenderServiceClient genderServiceClient;

    @Autowired
    public void setGenderServiceClient(GenderServiceClient genderServiceClient) {
        this.genderServiceClient = genderServiceClient;
    }

    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(String.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        String objectName = errors.getObjectName();
        String genderId = (String) target;
        log.debug("Validating genderId: {}", genderId);
        GenderVo genderVo = null;
        log.info("Requesting details of gender with id: {}", genderId);
        genderVo = genderServiceClient.getGenderDetailsById(genderId);
        log.info("Retrieved gender: {} by id", genderVo);
        if(genderVo == null) {
            errors.reject(CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            log.debug(objectName + ".genderId is invalid");
            return;
        }
        boolean emptyFileName = !StringUtils.hasText(StringUtils.trimWhitespace(genderVo.getName()));
        boolean emptyId = !StringUtils.hasText(StringUtils.trimWhitespace(genderVo.getId()));
        if(emptyFileName) {
            log.debug(objectName + ".gender.name is invalid");
            errors.reject(CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            return;
        }
        if(emptyId) {
            log.debug(objectName + ".gender.genderId is invalid");
            errors.reject(CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            return;
        }
        if(!genderVo.getActive()) {
            errors.reject(CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            log.debug(objectName + ".genderId is not active");
            return;
        }
    }

}
