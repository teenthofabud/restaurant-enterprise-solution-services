package com.teenthofabud.restaurant.solution.engagement.checkin.service.impl;

import com.teenthofabud.restaurant.solution.engagement.checkin.converter.*;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.*;
import com.teenthofabud.restaurant.solution.engagement.checkin.mapper.ReservationEntitySelfMapper;
import com.teenthofabud.restaurant.solution.engagement.checkin.mapper.ReservationForm2EntityMapper;
import com.teenthofabud.restaurant.solution.engagement.checkin.repository.ReservationRepository;
import com.teenthofabud.restaurant.solution.engagement.checkin.service.ReservationService;
import com.teenthofabud.restaurant.solution.engagement.checkin.validator.*;
import com.teenthofabud.restaurant.solution.engagement.constants.CheckInType;
import com.teenthofabud.restaurant.solution.engagement.constants.EngagementErrorCode;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Example;
import org.springframework.data.domain.ExampleMatcher;
import org.springframework.util.StringUtils;

import java.time.LocalDate;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.*;

@Slf4j
public class ReservationServiceImpl extends CheckInServiceImpl<ReservationFormValidator, ReservationFormRelaxedValidator, ReservationDtoValidator, ReservationRepository,
        ReservationEntitySelfMapper, ReservationForm2EntityMapper, ReservationForm2EntityConverter, ReservationEntity2VoConverter, ReservationDto2EntityConverter>
        implements ReservationService<ReservationForm, ReservationVo> {
    private String reservationTimeFormat;
    private String reservationDateFormat;

    @Value("${res.engagement.checkIn.reservation.time.format}")
    public void setReservationTimeFormat(String reservationTimeFormat) {
        this.reservationTimeFormat = reservationTimeFormat;
    }

    @Value("${res.engagement.checkIn.reservation.date.format}")
    public void setReservationDateFormat(String reservationDateFormat) {
        this.reservationDateFormat = reservationDateFormat;
    }

    @Override
    public ReservationFormValidator getCheckInFormValidator() {
        return (ReservationFormValidator) this.checkInBeanFactory.getCheckInFormValidator(CheckInType.RESERVATION.name()).get();
    }

    @Override
    public ReservationFormRelaxedValidator getCheckInFormRelaxedValidator() {
        return (ReservationFormRelaxedValidator) this.checkInBeanFactory.getCheckInFormRelaxedValidator(CheckInType.RESERVATION.name()).get();
    }

    @Override
    public ReservationDtoValidator getCheckInDtoValidator() {
        return (ReservationDtoValidator) this.checkInBeanFactory.getCheckInDtoValidator(CheckInType.RESERVATION.name()).get();
    }

    @Override
    public ReservationRepository getCheckInRepository() {
        return (ReservationRepository) this.checkInBeanFactory.getCheckInRepository(CheckInType.RESERVATION.name()).get();
    }

    @Override
    public ReservationEntitySelfMapper getCheckInEntitySelfMapper() {
        return (ReservationEntitySelfMapper) this.checkInBeanFactory.getCheckInEntitySelfMapper(CheckInType.RESERVATION.name()).get();
    }

    @Override
    public ReservationForm2EntityMapper getCheckInForm2EntityMapper() {
        return (ReservationForm2EntityMapper) this.checkInBeanFactory.getCheckInForm2EntityMapper(CheckInType.RESERVATION.name()).get();
    }

    @Override
    public ReservationForm2EntityConverter getCheckInForm2EntityConverter() {
        return (ReservationForm2EntityConverter) this.checkInBeanFactory.getCheckInForm2EntityConverter(CheckInType.RESERVATION.name()).get();
    }

    @Override
    public ReservationEntity2VoConverter getCheckInEntity2VoConverter() {
        return (ReservationEntity2VoConverter) this.checkInBeanFactory.getCheckInEntity2VoConverter(CheckInType.RESERVATION.name()).get();
    }

    @Override
    public ReservationDto2EntityConverter getCheckInDto2EntityConverter() {
        return (ReservationDto2EntityConverter) this.checkInBeanFactory.getCheckInDto2EntityConverter(CheckInType.RESERVATION.name()).get();
    }

    @Override
    public List<ReservationVo> retrieveAllMatchingReservationDetailsByCriteria(Optional<String> optionalDate, Optional<String> optionalTime) throws CheckInException {
        if(optionalDate.isEmpty() && optionalTime.isEmpty()) {
            log.debug("No search parameters provided");
        }
        String date = optionalDate.isPresent() ? optionalDate.get() : "";
        String time = optionalTime.isPresent() ? optionalTime.get() : "";
        if(StringUtils.isEmpty(StringUtils.trimWhitespace(date)) && StringUtils.isEmpty(StringUtils.trimWhitespace(time))) {
            log.debug("All search parameters are empty");
        }
        List<ReservationVo> matchedReservationList = new LinkedList<>();
        Map<String, String> providedFilters = new LinkedHashMap<>();
        ReservationEntity entity = new ReservationEntity(new CheckInEntity());
        ExampleMatcher matcherCriteria = ExampleMatcher.matchingAll();
        if(StringUtils.hasText(StringUtils.trimWhitespace(date))) {
            try {
                DateTimeFormatter dtf = DateTimeFormatter.ofPattern(reservationDateFormat);
                LocalDate dt = LocalDate.parse(date, dtf);
                log.debug("date {} is valid", date);
                providedFilters.put("date", date);
                entity.setDate(dt);
                matcherCriteria = matcherCriteria.withMatcher("date", match -> match.exact());
            } catch (DateTimeParseException e) {
                log.error("Unable to parse date", e);
                log.debug("Reservation date: {} is invalid", date);
                throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID, new Object[] { "date", date });
            }
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(time))) {
            try {
                DateTimeFormatter dtf = DateTimeFormatter.ofPattern(reservationTimeFormat);
                LocalTime t = LocalTime.parse(date, dtf);
                log.debug("time {} is valid", date);
                providedFilters.put("time", date);
                entity.setTime(t);
                matcherCriteria = matcherCriteria.withMatcher("time", match -> match.exact());
            } catch (DateTimeParseException e) {
                log.error("Unable to parse time", e);
                log.debug("Reservation time: {} is invalid", date);
                throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID, new Object[] { "time", date });
            }
        }
        if(providedFilters.isEmpty()) {
            log.debug("search parameters are not valid");
        } else {
            log.debug("search parameters {} are valid", providedFilters);
        }
        Example<ReservationEntity> reservationEntityExample = Example.of(entity, matcherCriteria);
        List<ReservationEntity> reservationEntityList = this.getCheckInRepository().findAll(reservationEntityExample);
        matchedReservationList = super.engagementServiceHelper.reservationEntity2DetailedVo(reservationEntityList);
        log.info("Found {} ReservationVo matching with provided parameters : {}", matchedReservationList.size(), providedFilters);
        log.info("No ReservationVo available matching with provided parameters : {}", matchedReservationList.size(), providedFilters);
        return matchedReservationList;
    }
}