package com.teenthofabud.restaurant.solution.reservation.booking.converter;

import com.teenthofabud.restaurant.solution.reservation.booking.data.BookingDocument;
import com.teenthofabud.restaurant.solution.reservation.booking.data.BookingForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;

@Component
@Slf4j
public class BookingForm2DocumentConverter implements Converter<BookingForm, BookingDocument> {

    private List<String> fieldsToEscape;
    private String bookingTimeFormat;

    @Value("#{'${res.reservation.booking.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Value("${res.reservation.booking.timestamp}")
    public void setBookingTimeFormat(String bookingTimeFormat) {
        this.bookingTimeFormat = bookingTimeFormat;
    }

    @Override
    public BookingDocument convert(BookingForm form) {
        BookingDocument document = new BookingDocument();
        if(!fieldsToEscape.contains("categoryId")) {
            document.setCategoryId(form.getCategoryId());
        }
        /*if(!fieldsToEscape.contains("tableId")) {
            document.setTableId(form.getTableId());
        }*/
        if(!fieldsToEscape.contains("timestamp")) {
            document.setTimestamp(LocalDateTime.parse(form.getTimestamp(),DateTimeFormatter.ofPattern(bookingTimeFormat)));
        }
        /*if(!fieldsToEscape.contains("noOfPerson")) {
            document.setNoOfPerson(form.getNoOfPerson());
        }*/
        if(!fieldsToEscape.contains("accountId")) {
            document.setAccountId(form.getAccountId());
        }
        document.setActive(Boolean.TRUE);
        log.debug("Converting {} to {}", form, document);
        return document;
    }

}
