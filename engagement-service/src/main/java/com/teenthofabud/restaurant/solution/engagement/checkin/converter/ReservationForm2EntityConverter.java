package com.teenthofabud.restaurant.solution.engagement.checkin.converter;

import com.teenthofabud.restaurant.solution.engagement.checkin.data.*;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.time.LocalDate;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.util.List;

@Component
@Slf4j
public class ReservationForm2EntityConverter extends CheckInForm2EntityConverter<ReservationForm, ReservationEntity>{

    private List<String> fieldsToEscape;
    private String reservationDateFormat;
    private String reservationTimeFormat;

    @Value("${res.engagement.checkIn.reservation.time.format}")
    public void setReservationTimeFormat(String reservationTimeFormat) {
        this.reservationTimeFormat = reservationTimeFormat;
    }

    @Value("${res.engagement.checkIn.reservation.date.format}")
    public void setReservationDateFormat(String reservationDateFormat) {
        this.reservationDateFormat = reservationDateFormat;
    }

    @Value("#{'${res.engagement.checkIn.reservation.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public List<String> getFieldsToEscape() {
        return this.fieldsToEscape;
    }

    @Override
    public ReservationEntity convert(ReservationForm form) {
        ReservationEntity entity = new ReservationEntity();
        super.convert(form, entity);
        if(!fieldsToEscape.contains("date")) {
            entity.setDate(LocalDate.parse(form.getDate(), DateTimeFormatter.ofPattern(reservationDateFormat)));
        }
        if(!fieldsToEscape.contains("time")) {
            entity.setTime(LocalTime.parse(form.getTime(), DateTimeFormatter.ofPattern(reservationTimeFormat)));
        }
        entity.setActive(Boolean.TRUE);
        log.debug("Converting {} to {}", form, entity);
        return entity;
    }
}
