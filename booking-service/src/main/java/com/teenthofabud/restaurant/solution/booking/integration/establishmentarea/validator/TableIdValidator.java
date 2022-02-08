package com.teenthofabud.restaurant.solution.booking.integration.establishmentarea.validator;

import com.teenthofabud.restaurant.solution.booking.error.BookingErrorCode;
import com.teenthofabud.restaurant.solution.booking.integration.establishmentarea.data.TableVo;
import com.teenthofabud.restaurant.solution.booking.integration.establishmentarea.proxy.EstablishmentAreaServiceClient;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

@Component
@Slf4j
public class TableIdValidator implements Validator {

    private EstablishmentAreaServiceClient establishmentAreaServiceClient;

    @Autowired
    public void setTableServiceClient(EstablishmentAreaServiceClient establishmentAreaServiceClient) {
        this.establishmentAreaServiceClient = establishmentAreaServiceClient;
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
        tableVo = establishmentAreaServiceClient.getTableDetailsById(tableId);
        log.info("Retrieved table: {} by id", tableVo);
        if(tableVo == null) {
            errors.reject(BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.name());
            log.debug(objectName + ".tableId is invalid");
            return;
        }
        boolean emptyTableName = !StringUtils.hasText(StringUtils.trimWhitespace(tableVo.getTableName()));
        boolean emptyTableId = !StringUtils.hasText(StringUtils.trimWhitespace(tableVo.getTableId()));
        if(emptyTableName) {
            log.debug(objectName + ".table.name is invalid");
            errors.reject(BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.name());
            return;
        }
        if(emptyTableId) {
            log.debug(objectName + ".table.tableId is invalid");
            errors.reject(BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.name());
            return;
        }
        if(!tableVo.getActive()) {
            errors.reject(BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.name());
            log.debug(objectName + ".tableId is not active");
            return;
        }
    }

}
