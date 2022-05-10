package com.teenthofabud.restaurant.solution.reservation.booking.converter;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.converter.TOABBaseDocument2VoConverter;
import com.teenthofabud.core.common.data.dto.TOABRequestContextHolder;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.restaurant.solution.reservation.booking.data.BookingDocument;
import com.teenthofabud.restaurant.solution.reservation.booking.data.BookingVo;
import com.teenthofabud.restaurant.solution.reservation.category.data.CategoryVo;
import com.teenthofabud.restaurant.solution.reservation.category.service.CategoryService;
import com.teenthofabud.restaurant.solution.reservation.integration.customer.data.AccountVo;
import com.teenthofabud.restaurant.solution.reservation.integration.customer.proxy.CustomerServiceClient;
import com.teenthofabud.restaurant.solution.reservation.integration.establishmentarea.data.TableVo;
import com.teenthofabud.restaurant.solution.reservation.integration.establishmentarea.proxy.EstablishmentAreaServiceClient;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.convert.converter.Converter;
import org.springframework.scheduling.concurrent.CustomizableThreadFactory;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.*;

@Component
@Slf4j
public class BookingDocument2VoConverter extends TOABBaseDocument2VoConverter<BookingDocument, BookingVo> implements Converter<BookingDocument, BookingVo> {

    private List<String> fieldsToEscape;
    //private EstablishmentAreaServiceClient establishmentAreaServiceClient;
    private CustomerServiceClient customerServiceClient;
    private CategoryService categoryService;
    private String bookingTimeFormat;

    @Value("#{'${res.reservation.booking.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    /*@Autowired
    public void setEstablishmentAreaServiceClient(EstablishmentAreaServiceClient establishmentAreaServiceClient) {
        this.establishmentAreaServiceClient = establishmentAreaServiceClient;
    }*/

    @Value("${res.reservation.booking.timestamp}")
    public void setBookingTimeFormat(String bookingTimeFormat) {
        this.bookingTimeFormat = bookingTimeFormat;
    }


    @Autowired
    public void setCustomerServiceClient(CustomerServiceClient customerServiceClient) {
        this.customerServiceClient = customerServiceClient;
    }

    @Autowired
    public void setExperienceService(CategoryService categoryService) {
        this.categoryService = categoryService;
    }

    @Override
    public BookingVo convert(BookingDocument document) {
        BookingVo vo = new BookingVo();
        if(!fieldsToEscape.contains("id")) {
            vo.setId(document.getId().toString());
        }
        if(!fieldsToEscape.contains("timestamp")) {
            vo.setTimestamp(document.getTimestamp().format(DateTimeFormatter.ofPattern(bookingTimeFormat)));
        }
        /*if(!fieldsToEscape.contains("noOfPerson")) {
            vo.setNoOfPerson(document.getNoOfPerson());
        }*/
        if(!fieldsToEscape.contains("categoryId")) {
            this.expandSecondLevelFields(document, vo, "categoryId");
        }
        /*if(!fieldsToEscape.contains("tableId")) {
            this.expandSecondLevelFields(document, vo, "tableId");
        }*/
        if(!fieldsToEscape.contains("accountId")) {
            this.expandSecondLevelFields(document, vo, "accountId");
        }
        super.expandAuditFields(document, vo);
        log.debug("Converted {} to {} ", document, vo);
        return vo;
    }

    private void expandSecondLevelFields(BookingDocument document, BookingVo vo, String fieldName) {
        TOABCascadeLevel cascadeLevel = TOABRequestContextHolder.getCascadeLevelContext();
        switch(cascadeLevel) {
            case TWO:
                if(!fieldsToEscape.contains("categoryId") && fieldName.compareTo("categoryId") == 0) {
                    Callable<CategoryVo> categoryDocument2VoConversion = () -> {
                        CategoryVo recipeVo = categoryService.retrieveDetailsById(document.getCategoryId(), Optional.of(TOABCascadeLevel.ZERO));
                        return recipeVo;
                    };
                    String tName = "categoryDocument2VoConversion";
                    ExecutorService executorService = Executors.newFixedThreadPool(1, new CustomizableThreadFactory(tName + "-"));
                    Future<CategoryVo> categoryDocument2VoConversionResult = executorService.submit(categoryDocument2VoConversion);
                    try {
                        CategoryVo categoryVo = categoryDocument2VoConversionResult.get();
                        vo.setCategory(categoryVo);
                        log.debug("Retrieved {} for categoryId: {}", categoryVo, document.getCategoryId());
                    } catch (InterruptedException | ExecutionException e) {
                        String msg = "Unable to perform " + tName;
                        log.error(msg, e);
                        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, msg, new Object[] { tName + " failure: " + e.getMessage() });
                    }
                }
                if(!fieldsToEscape.contains("accountId") && fieldName.compareTo("accountId") == 0) {
                    Callable<AccountVo> accountDocument2VoConversion = () -> {
                        AccountVo accountVo = customerServiceClient.getAccountDetailsById(document.getAccountId());
                        return accountVo;
                    };
                    String tName = "accountDocument2VoConversion";
                    ExecutorService executorService = Executors.newFixedThreadPool(1, new CustomizableThreadFactory(tName + "-"));
                    Future<AccountVo> accountDocument2VoConversionResult = executorService.submit(accountDocument2VoConversion);
                    try {
                        AccountVo accountVo = accountDocument2VoConversionResult.get();
                        vo.setAccount(accountVo);
                        log.debug("Retrieved {} for accountId: {}", accountVo, document.getCategoryId());
                    } catch (InterruptedException | ExecutionException e) {
                        String msg = "Unable to perform " + tName;
                        log.error(msg, e);
                        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, msg, new Object[] { tName + " failure: " + e.getMessage() });
                    }
                }
                /*if(!fieldsToEscape.contains("tableId") && fieldName.compareTo("tableId") == 0) {
                    TableVo table = establishmentAreaServiceClient.getTableDetailsById(document.getTableId());
                    log.debug("Retrieved {} for table is: {}", table, document.getTableId());
                    vo.setTable(table);
                }*/
                break;
            default:
                vo.setCategoryId(document.getCategoryId());
                //vo.setTableId(document.getTableId());
                vo.setAccountId(document.getAccountId());
                log.debug("only first level cascaded for booking over categoryId and accountId");
                break;
        }
    }

}
