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

    public boolean validateCheckInId(String checkInId, Errors errors) {
        boolean flag = true;
        boolean reservationFlag = this.validateCheckInId(checkInId, reservationService, errors);
        boolean walkInFlag = this.validateCheckInId(checkInId, walkInService, errors);
        if(!reservationFlag && !walkInFlag) {
            log.debug("{}.checkInId does not match for both Reservation and WalkIn", errors.getObjectName());
            flag = false;
        }
        return flag;
    }

    private boolean validateCheckInId(String checkInId, CheckInService checkInService, Errors errors) {
        boolean flag = true;
        try {
            CheckInVo checkInVo = checkInService.retrieveDetailsById(checkInId, Optional.of(TOABCascadeLevel.TWO));
            if(!checkInVo.getActive()) {
                log.debug("{}.checkInId is inactive", errors.getObjectName());
                errors.rejectValue("checkInId", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
                flag = false;
            }
        } catch (CheckInException e) {
            log.debug("{}.checkInId is invalid", errors.getObjectName());
            errors.rejectValue("checkInId", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            flag = false;
        } finally {
            return flag;
        }
    }

}
