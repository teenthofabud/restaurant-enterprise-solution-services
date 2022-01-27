package com.teenthofabud.restaurant.solution.session.integration.customer.account.validator;

import com.teenthofabud.restaurant.solution.customer.account.data.AccountVo;
import com.teenthofabud.restaurant.solution.session.error.SessionErrorCode;
import com.teenthofabud.restaurant.solution.session.integration.customer.account.proxy.AccountServiceClient;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

@Component
@Slf4j
public class AccountIdValidator implements Validator {

    private AccountServiceClient accountServiceClient;

    @Autowired
    public void setAccountServiceClient(AccountServiceClient accountServiceClient) {
        this.accountServiceClient = accountServiceClient;
    }

    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(String.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        String objectName = errors.getObjectName();
        String accountId = (String) target;
        log.debug("Validating accountId: {}", accountId);
        AccountVo accountVo = null;
        log.info("Requesting details of account with id: {}", accountId);
        accountVo = accountServiceClient.getAccountDetailsById(accountId);
        log.info("Retrieved account: {} by id", accountVo);
        if(accountVo == null) {
            errors.reject(SessionErrorCode.SESSION_ATTRIBUTE_INVALID.name());
            log.debug(objectName + ".accountId is invalid");
            return;
        }
        boolean emptyFirstName = !StringUtils.hasText(StringUtils.trimWhitespace(accountVo.getFirstName()));
        boolean emptyLastName = !StringUtils.hasText(StringUtils.trimWhitespace(accountVo.getLastName()));
        boolean emptyId = !StringUtils.hasText(StringUtils.trimWhitespace(accountVo.getId()));
        if(emptyFirstName) {
            log.debug(objectName + ".account.firstName is invalid");
            errors.reject(SessionErrorCode.SESSION_ATTRIBUTE_INVALID.name());
            return;
        }
        if(emptyLastName) {
            log.debug(objectName + ".account.lastName is invalid");
            errors.reject(SessionErrorCode.SESSION_ATTRIBUTE_INVALID.name());
            return;
        }
        if(emptyId) {
            log.debug(objectName + ".account.accountId is invalid");
            errors.reject(SessionErrorCode.SESSION_ATTRIBUTE_INVALID.name());
            return;
        }
        if(!accountVo.getActive()) {
            errors.reject(SessionErrorCode.SESSION_ATTRIBUTE_INVALID.name());
            log.debug(objectName + ".accountId is not active");
            return;
        }
    }

}
