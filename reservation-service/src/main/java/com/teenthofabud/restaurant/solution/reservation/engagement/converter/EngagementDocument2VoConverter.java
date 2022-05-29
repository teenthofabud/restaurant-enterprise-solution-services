package com.teenthofabud.restaurant.solution.reservation.engagement.converter;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.converter.TOABBaseDocument2VoConverter;
import com.teenthofabud.core.common.data.dto.TOABRequestContextHolder;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.restaurant.solution.reservation.category.data.CategoryVo;
import com.teenthofabud.restaurant.solution.reservation.booking.data.BookingVo;
import com.teenthofabud.restaurant.solution.reservation.booking.service.BookingService;
import com.teenthofabud.restaurant.solution.reservation.engagement.data.DeliveryEngagementDocument;
import com.teenthofabud.restaurant.solution.reservation.engagement.data.DineInEngagementDocument;
import com.teenthofabud.restaurant.solution.reservation.engagement.data.EngagementDocument;
import com.teenthofabud.restaurant.solution.reservation.engagement.data.EngagementVo;
import com.teenthofabud.restaurant.solution.reservation.engagement.data.TakeAwayEngagementDocument;
import com.teenthofabud.restaurant.solution.reservation.integration.establishmentarea.data.TableVo;
import com.teenthofabud.restaurant.solution.reservation.integration.establishmentarea.proxy.EstablishmentAreaServiceClient;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.convert.converter.Converter;
import org.springframework.scheduling.concurrent.CustomizableThreadFactory;
import org.springframework.stereotype.Component;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

@Component
@Slf4j
public class EngagementDocument2VoConverter extends TOABBaseDocument2VoConverter<EngagementDocument, EngagementVo> implements Converter<EngagementDocument, EngagementVo> {

    private List<String> fieldsToEscape;
    private BookingService bookingService;
    private EstablishmentAreaServiceClient establishmentAreaServiceClient;


    @Value("#{'${res.reservation.engagement.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Autowired
    public void setBookingService(BookingService bookingService) {
        this.bookingService = bookingService;
    }

    @Autowired
    public void setEstablishmentAreaServiceClient(EstablishmentAreaServiceClient establishmentAreaServiceClient) {
        this.establishmentAreaServiceClient = establishmentAreaServiceClient;
    }

    @Override
    public EngagementVo convert(EngagementDocument document) {
        EngagementVo vo = new EngagementVo();
        if(!fieldsToEscape.contains("id")) {
            vo.setId(document.getId());
        }
        if(!fieldsToEscape.contains("bookingId")) {
            this.expandSecondLevelFields(document, vo, "bookingId");
        }
        if(!fieldsToEscape.contains("tokenNumber")) {
            vo.setTokenNumber(document.getTokenNumber());
        }
        Optional<BookingVo> optionalBookingVo = this.getBooking(document.getBookingId());
        CategoryVo categoryVo = optionalBookingVo.get().getCategory();
        switch (categoryVo.getName()) {
            case "Dine In":
                this.expandSecondLevelFields(document, vo, "tableId");
                break;
            case "Take Away":
                vo.setInstructions(((TakeAwayEngagementDocument) document).getInstructions());
                break;
            case "Delivery":
                vo.setExtRef(((DeliveryEngagementDocument) document).getExtRef());
                break;
            default:
                String msg = "Category not supported";
                log.error(msg + ": {}", categoryVo.getName());
                throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, msg, new Object[] { categoryVo.getName() });
        }
        super.expandAuditFields(document, vo);
        log.debug("Converted {} to {} ", document, vo);
        return vo;
    }

    private Optional<BookingVo> getBooking(String bookingId) {
        Optional<BookingVo> optionalBookingVo = Optional.empty();
        Callable<BookingVo> bookingDocument2VoConversion = () -> {
            BookingVo recipeVo = bookingService.retrieveDetailsById(bookingId, Optional.of(TOABCascadeLevel.ZERO));
            return recipeVo;
        };
        String tName = "bookingDocument2VoConversion";
        ExecutorService executorService = Executors.newFixedThreadPool(1, new CustomizableThreadFactory(tName + "-"));
        Future<BookingVo> bookingDocument2VoConversionResult = executorService.submit(bookingDocument2VoConversion);
        try {
            BookingVo bookingVo = bookingDocument2VoConversionResult.get();
            log.debug("Retrieved {} for bookingId: {}", bookingVo, bookingId);
            optionalBookingVo = Optional.of(bookingVo);
        } catch (InterruptedException | ExecutionException e) {
            String msg = "Unable to perform " + tName;
            log.error(msg, e);
            throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, msg, new Object[] { tName + " failure: " + e.getMessage() });
        }
        return optionalBookingVo;
    }

    private Optional<TableVo> getTable(DineInEngagementDocument document) {
        Optional<TableVo> optionalTableVo = Optional.empty();
        Callable<TableVo> tableDocument2VoConversion = () -> {
            TableVo tableVo = establishmentAreaServiceClient.getTableDetailsById(document.getTableId());
            return tableVo;
        };
        String tName = "tableDocument2VoConversion";
        ExecutorService executorService = Executors.newFixedThreadPool(1, new CustomizableThreadFactory(tName + "-"));
        Future<TableVo> tableDocument2VoConversionResult = executorService.submit(tableDocument2VoConversion);
        try {
            TableVo tableVo = tableDocument2VoConversionResult.get();
            log.debug("Retrieved {} for tableId: {}", tableVo, document.getTableId());
            optionalTableVo = Optional.of(tableVo);
        } catch (InterruptedException | ExecutionException e) {
            String msg = "Unable to perform " + tName;
            log.error(msg, e);
            throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, msg, new Object[] { tName + " failure: " + e.getMessage() });
        }
        return optionalTableVo;
    }

    private void expandSecondLevelFields(EngagementDocument document, EngagementVo vo, String fieldName) {
        TOABCascadeLevel cascadeLevel = TOABRequestContextHolder.getCascadeLevelContext();
        switch(cascadeLevel) {
            case TWO:
                if(!fieldsToEscape.contains("bookingId") && fieldName.compareTo("bookingId") == 0) {
                    Optional<BookingVo> optionalBookingVo = this.getBooking(document.getBookingId());
                    vo.setBooking(optionalBookingVo.get());
                }
                if(!fieldsToEscape.contains("tableId") && fieldName.compareTo("tableId") == 0) {
                    Optional<TableVo> optionalTableVo = this.getTable((DineInEngagementDocument) document);
                    vo.setTable(optionalTableVo.get());
                }
                break;
            default:
                vo.setBookingId(document.getBookingId());
                vo.setTableId(((DineInEngagementDocument) document).getTableId());
                log.debug("only first level cascaded for engagement over bookingId");
                break;
        }
    }

}
