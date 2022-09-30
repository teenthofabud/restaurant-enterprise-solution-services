package com.teenthofabud.restaurant.solution.engagement.checkin.mapper;

import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInEntity;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInForm;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.ReservationEntity;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.ReservationForm;
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
public class ReservationForm2EntityMapper extends CheckInForm2EntityMapper {

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

    @Override
    public Optional<? extends CheckInEntity> compareAndMap(Optional<? extends CheckInEntity> optionalCheckInEntityChild, CheckInForm form) {
        ReservationEntity actualEntity = (ReservationEntity) optionalCheckInEntityChild.get();
        ReservationForm checkInFormParameters = (ReservationForm) form.getAttributes();
        boolean changeSW = false;
        // direct copy of common attributes handled in parent
        Optional<CheckInEntity> expectedParentEntity = super.compareAndMap(actualEntity, form);
        ReservationEntity expectedEntity = expectedParentEntity.isPresent() ? (ReservationEntity) expectedParentEntity.get() : new ReservationEntity();
        // direct copy
        expectedEntity.setId(actualEntity.getId());
        log.debug("Directly copying ReservationEntity.id: {} from actualEntity to expectedEntity", actualEntity.getId());


        if(!fieldsToEscape.contains("date") && StringUtils.hasText(StringUtils.trimWhitespace(checkInFormParameters.getDate()))) {
            LocalDate date = LocalDate.parse(checkInFormParameters.getDate(), DateTimeFormatter.ofPattern(reservationDateFormat));
            if(date.compareTo(actualEntity.getDate()) != 0) {
                expectedEntity.setDate(date);
                changeSW = true;
                log.debug("ReservationForm.date: {} is different as ReservationEntity.date: {}", checkInFormParameters.getDate(), actualEntity.getDate());
            } else {
                expectedEntity.setDate(actualEntity.getDate());
                log.debug("ReservationForm.date: is unchanged");
            }
        } else {
            expectedEntity.setDate(actualEntity.getDate());
            log.debug("ReservationForm.date: is unchanged");
        }

        if(!fieldsToEscape.contains("time") && StringUtils.hasText(StringUtils.trimWhitespace(checkInFormParameters.getTime()))) {
            LocalTime time = LocalTime.parse(checkInFormParameters.getDate(), DateTimeFormatter.ofPattern(reservationTimeFormat));
            if(time.compareTo(actualEntity.getTime()) != 0) {
                expectedEntity.setTime(time);
                changeSW = true;
                log.debug("ReservationForm.time: {} is different as ReservationEntity.time: {}", checkInFormParameters.getTime(), actualEntity.getTime());
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
