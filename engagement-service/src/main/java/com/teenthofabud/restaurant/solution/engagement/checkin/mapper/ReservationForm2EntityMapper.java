package com.teenthofabud.restaurant.solution.engagement.checkin.mapper;

import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.*;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.time.LocalDate;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class ReservationForm2EntityMapper extends CheckInForm2EntityMapper<ReservationEntity, ReservationForm> {

    private List<String> fieldsToEscape;
    private String reservationTimeFormat;
    private String reservationDateFormat;

    @Value("#{'${res.engagement.checkIn.reservation.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Value("${res.engagement.checkIn.reservation.time.format}")
    public void setReservationTimeFormat(String reservationTimeFormat) {
        this.reservationTimeFormat = reservationTimeFormat;
    }

    @Value("${res.engagement.checkIn.reservation.date.format}")
    public void setReservationDateFormat(String reservationDateFormat) {
        this.reservationDateFormat = reservationDateFormat;
    }

    /*@Override
    protected Optional<ReservationEntity> compareAndMap(ReservationEntity parent, ReservationEntity checkInEntityChild, ReservationForm checkInFormChild) {
        ReservationEntity actualEntity = checkInEntityChild;
        ReservationForm form = checkInFormChild;
        boolean changeSW = false;

        ReservationEntity expectedEntity = new ReservationEntity(parent);
        // direct copy
        expectedEntity.setId(actualEntity.getId());
        log.debug("Directly copying ReservationEntity.id: {} from actualEntity to expectedEntity", actualEntity.getId());


        if(!fieldsToEscape.contains("date") && StringUtils.hasText(StringUtils.trimWhitespace(form.getDate()))) {
            LocalDate date = LocalDate.parse(form.getDate(), DateTimeFormatter.ofPattern(reservationDateFormat));
            if(date.compareTo(actualEntity.getDate()) != 0) {
                expectedEntity.setDate(date);
                changeSW = true;
                log.debug("ReservationForm.date: {} is different as ReservationEntity.date: {}", form.getDate(), actualEntity.getDate());
            } else {
                expectedEntity.setDate(actualEntity.getDate());
                log.debug("ReservationForm.date: is unchanged");
            }
        } else {
            expectedEntity.setDate(actualEntity.getDate());
            log.debug("ReservationForm.date: is unchanged");
        }

        if(!fieldsToEscape.contains("time") && StringUtils.hasText(StringUtils.trimWhitespace(form.getTime()))) {
            LocalTime time = LocalTime.parse(form.getDate(), DateTimeFormatter.ofPattern(reservationTimeFormat));
            if(time.compareTo(actualEntity.getTime()) != 0) {
                expectedEntity.setTime(time);
                changeSW = true;
                log.debug("ReservationForm.time: {} is different as ReservationEntity.time: {}", form.getTime(), actualEntity.getTime());
            } else {
                expectedEntity.setDate(actualEntity.getDate());
                log.debug("ReservationForm.time: is unchanged");
            }
        } else {
            expectedEntity.setTime(actualEntity.getTime());
            log.debug("ReservationForm.time: is unchanged");
        }

        return changeSW ? Optional.of(expectedEntity) : Optional.empty();
    }*/

    @Override
    public Optional<ReservationEntity> compareAndMap(ReservationEntity actualEntity, ReservationForm form) {
        boolean changeSW = false;

        ReservationEntity expectedEntity = new ReservationEntity();
        // direct copy
        expectedEntity.setId(actualEntity.getId());
        log.debug("Directly copying ReservationEntity.id: {} from actualEntity to expectedEntity", actualEntity.getId());

        // parent copy
        Optional<ReservationEntity> optionalExpectedEntity = super.compareAndMap(actualEntity, expectedEntity, form);
        if(optionalExpectedEntity.isPresent()) {
            expectedEntity = optionalExpectedEntity.get();
        }

        if(!fieldsToEscape.contains("date") && StringUtils.hasText(StringUtils.trimWhitespace(form.getDate()))) {
            LocalDate date = LocalDate.parse(form.getDate(), DateTimeFormatter.ofPattern(reservationDateFormat));
            if(date.compareTo(actualEntity.getDate()) != 0) {
                expectedEntity.setDate(date);
                changeSW = true;
                log.debug("ReservationForm.date: {} is different as ReservationEntity.date: {}", form.getDate(), actualEntity.getDate());
            } else {
                expectedEntity.setDate(actualEntity.getDate());
                log.debug("ReservationForm.date: is unchanged");
            }
        } else {
            expectedEntity.setDate(actualEntity.getDate());
            log.debug("ReservationForm.date: is unchanged");
        }

        if(!fieldsToEscape.contains("time") && StringUtils.hasText(StringUtils.trimWhitespace(form.getTime()))) {
            LocalTime time = LocalTime.parse(form.getDate(), DateTimeFormatter.ofPattern(reservationTimeFormat));
            if(time.compareTo(actualEntity.getTime()) != 0) {
                expectedEntity.setTime(time);
                changeSW = true;
                log.debug("ReservationForm.time: {} is different as ReservationEntity.time: {}", form.getTime(), actualEntity.getTime());
            } else {
                expectedEntity.setDate(actualEntity.getDate());
                log.debug("ReservationForm.time: is unchanged");
            }
        } else {
            expectedEntity.setTime(actualEntity.getTime());
            log.debug("ReservationForm.time: is unchanged");
        }

        return changeSW ? Optional.of(expectedEntity) : Optional.empty();
    }
}
