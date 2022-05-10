package com.teenthofabud.restaurant.solution.reservation.utils;

import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.restaurant.solution.reservation.engagement.data.EngagementVo;
import com.teenthofabud.restaurant.solution.reservation.booking.converter.BookingDocument2VoConverter;
import com.teenthofabud.restaurant.solution.reservation.booking.data.BookingDocument;
import com.teenthofabud.restaurant.solution.reservation.booking.data.BookingVo;
//import com.teenthofabud.restaurant.solution.reservation.engagement.converter.EngagementDocument2VoConverter;
import com.teenthofabud.restaurant.solution.reservation.engagement.data.EngagementDocument;
import com.teenthofabud.restaurant.solution.reservation.category.converter.CategoryDocument2VoConverter;
import com.teenthofabud.restaurant.solution.reservation.category.data.CategoryDocument;
import com.teenthofabud.restaurant.solution.reservation.category.data.CategoryVo;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.LinkedList;
import java.util.List;

@Slf4j
@Component
public class ReservationServiceHelper {

    private CategoryDocument2VoConverter categoryDocument2VoConverter;
    private BookingDocument2VoConverter bookingDocument2VoConverter;
    //private EngagementDocument2VoConverter engagementDocument2VoConverter;


    @Autowired
    public void setBookingDocument2VoConverter(BookingDocument2VoConverter bookingDocument2VoConverter) {
        this.bookingDocument2VoConverter = bookingDocument2VoConverter;
    }

    @Autowired
    public void setCategoryDocument2VoConverter(CategoryDocument2VoConverter categoryDocument2VoConverter) {
        this.categoryDocument2VoConverter = categoryDocument2VoConverter;
    }

    /*@Autowired
    public void setEngagementDocument2VoConverter(EngagementDocument2VoConverter engagementDocument2VoConverter) {
        this.engagementDocument2VoConverter = engagementDocument2VoConverter;
    }*/

    public List<CategoryVo> categoryDocument2DetailedVo(List<? extends CategoryDocument> categoryDocumentList) throws TOABSystemException {
        List<CategoryVo> categoryDetailsList = new LinkedList<>();
        if(categoryDocumentList != null && !categoryDocumentList.isEmpty()) {
            for(CategoryDocument document : categoryDocumentList) {
                CategoryVo vo = this.categoryDocument2DetailedVo(document);
                categoryDetailsList.add(vo);
            }
        }
        return categoryDetailsList;
    }

    public CategoryVo categoryDocument2DetailedVo(CategoryDocument categoryDocument) throws TOABSystemException {
        if(categoryDocument != null) {
            CategoryVo vo = categoryDocument2VoConverter.convert(categoryDocument);
            log.debug("Converting {} to {}", categoryDocument, vo);
            return vo;
        }
        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, new Object[] { "category document is null" });
    }

    public List<BookingVo> bookingDocument2DetailedVo(List<? extends BookingDocument> bookingDocumentList) {
        List<BookingVo> bookingDetailsList = new LinkedList<>();
        if(bookingDocumentList != null && !bookingDocumentList.isEmpty()) {
            for(BookingDocument document : bookingDocumentList) {
                BookingVo vo = this.bookingDocument2DetailedVo(document);
                bookingDetailsList.add(vo);
            }
        }
        return bookingDetailsList;
    }

    public BookingVo bookingDocument2DetailedVo(BookingDocument bookingDocument) {
        if(bookingDocument != null) {
            BookingVo vo = bookingDocument2VoConverter.convert(bookingDocument);
            log.debug("Converting {} to {}", bookingDocument, vo);
            return vo;
        }
        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, new Object[] { "booking document is null" });
    }

    /*public List<EngagementVo> engagementDocument2DetailedVo(List<? extends EngagementDocument> engagementDocumentList) {
        List<EngagementVo> engagementDetailsList = new LinkedList<>();
        if(engagementDocumentList != null && !engagementDocumentList.isEmpty()) {
            for(EngagementDocument document : engagementDocumentList) {
                EngagementVo vo = this.engagementDocument2DetailedVo(document);
                engagementDetailsList.add(vo);
            }
        }
        return engagementDetailsList;
    }*/

    /*public EngagementVo engagementDocument2DetailedVo(EngagementDocument engagementDocument) {
        if(engagementDocument != null) {
            EngagementVo vo = engagementDocument2VoConverter.convert(engagementDocument);
            log.debug("Converting {} to {}", engagementDocument, vo);
            return vo;
        }
        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, new Object[] { "engagement document is null" });
    }*/
}
