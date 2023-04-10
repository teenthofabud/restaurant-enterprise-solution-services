package com.teenthofabud.restaurant.solution.engagement.tableallocation.validator;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInException;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInVo;
import com.teenthofabud.restaurant.solution.engagement.checkin.service.CheckInService;
import com.teenthofabud.restaurant.solution.engagement.checkin.service.ReservationService;
import com.teenthofabud.restaurant.solution.engagement.checkin.service.WalkInService;
import com.teenthofabud.restaurant.solution.engagement.constants.EngagementErrorCode;
import com.teenthofabud.restaurant.solution.engagement.utils.EngagementServiceHelper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.DirectFieldBindingResult;
import org.springframework.validation.Errors;

import java.util.Optional;

@Slf4j
public abstract class TableAllocationValidator {

    protected EngagementServiceHelper engagementServiceHelper;
    protected ReservationService reservationService;
    protected WalkInService walkInService;

    @Autowired
    public void setReservationService(ReservationService reservationService) {
        this.reservationService = reservationService;
    }

    @Autowired
    public void setWalkInService(WalkInService walkInService) {
        this.walkInService = walkInService;
    }


    @Autowired
    public void setEngagementServiceHelper(EngagementServiceHelper engagementServiceHelper) {
        this.engagementServiceHelper = engagementServiceHelper;
    }

    public boolean validateCheckInId(String checkInId, Class<?> type) {
        boolean flag = true;
        Errors reservationErrors = new DirectFieldBindingResult(checkInId, type.getSimpleName());
        Errors walkInErrors = new DirectFieldBindingResult(checkInId, type.getSimpleName());
        this.validateCheckInId(checkInId, reservationService, reservationErrors);
        this.validateCheckInId(checkInId, walkInService, walkInErrors);
        if(reservationErrors.getErrorCount() > 0 && walkInErrors.getErrorCount() > 0) {
            log.debug("{}.checkInId does not match for both Reservation and WalkIn", type.getSimpleName());
            flag = false;
        }
        return flag;
    }

    private void validateCheckInId(String checkInId, CheckInService checkInService, Errors errors) {
        try {
            CheckInVo checkInVo = checkInService.retrieveDetailsById(checkInId, Optional.of(TOABCascadeLevel.TWO));
            if(!checkInVo.getActive()) {
                log.debug("{}.checkInId is inactive", errors.getObjectName());
                errors.reject("checkInId", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            }
        } catch (CheckInException e) {
            log.error("{}.checkInId of {} is invalid", errors.getObjectName(), checkInService.getContextCheckInType(), e);
            errors.reject("checkInId", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
        }
    }

}
