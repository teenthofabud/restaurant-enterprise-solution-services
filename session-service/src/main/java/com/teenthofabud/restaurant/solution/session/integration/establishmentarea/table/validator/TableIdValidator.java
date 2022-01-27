package com.teenthofabud.restaurant.solution.session.integration.establishmentarea.table.validator;

import com.teenthofabud.restaurant.solution.session.error.SessionErrorCode;
import com.teenthofabud.restaurant.solution.session.integration.establishmentarea.table.data.TableVo;
import com.teenthofabud.restaurant.solution.session.integration.establishmentarea.table.proxy.TableServiceClient;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

@Component
@Slf4j
public class TableIdValidator implements Validator {

    private TableServiceClient tableServiceClient;

    @Autowired
    public void setTableServiceClient(TableServiceClient tableServiceClient) {
        this.tableServiceClient = tableServiceClient;
    }

    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(String.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        String objectName = errors.getObjectName();
        String tableId = (String) target;
        log.debug("Validating tableId: {}", tableId);
        TableVo tableVo = null;
        log.info("Requesting details of table with id: {}", tableId);
        tableVo = tableServiceClient.getTableDetailsById(tableId);
        log.info("Retrieved table: {} by id", tableVo);
        if(tableVo == null) {
            errors.reject(SessionErrorCode.SESSION_ATTRIBUTE_INVALID.name());
            log.debug(objectName + ".tableId is invalid");
            return;
        }
        boolean emptyTableName = !StringUtils.hasText(StringUtils.trimWhitespace(tableVo.getTableName()));
        boolean emptyTableId = !StringUtils.hasText(StringUtils.trimWhitespace(tableVo.getTableId()));
        if(emptyTableName) {
            log.debug(objectName + ".table.name is invalid");
            errors.reject(SessionErrorCode.SESSION_ATTRIBUTE_INVALID.name());
            return;
        }
        if(emptyTableId) {
            log.debug(objectName + ".table.tableId is invalid");
            errors.reject(SessionErrorCode.SESSION_ATTRIBUTE_INVALID.name());
            return;
        }
        if(!tableVo.getActive()) {
            errors.reject(SessionErrorCode.SESSION_ATTRIBUTE_INVALID.name());
            log.debug(objectName + ".tableId is not active");
            return;
        }
    }

}
