package com.teenthofabud.restaurant.solution.engagement.checkin.service;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInException;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInForm;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInVo;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;
import java.util.Set;

@Service
public interface CheckInService {

    public Set<CheckInVo> retrieveAllByNaturalOrdering();

    public CheckInVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws CheckInException;

    public List<CheckInVo> retrieveAllMatchingCheckInDetailsByCriteria(Optional<String> optionalAccountId,
                                                                //Optional<String> optionalTableId,
                                                                Optional<String> optionalSequence,
                                                                Optional<String> optionalNotes) throws CheckInException;

    public List<CheckInVo> retrieveAllMatchingWalkInDetailsByCriteria(Optional<String> optionalName,
                                                                Optional<String> optionalPhoneNumber,
                                                                Optional<String> optionalEmailId) throws CheckInException;

    public List<CheckInVo> retrieveAllMatchingReservationDetailsByCriteria(Optional<String> optionalDate,
                                                                Optional<String> optionalTime) throws CheckInException;

    public CheckInVo retrieveAllMatchingDetailsByCriteria(String sequence, String date) throws CheckInException;

    public String createCheckIn(CheckInForm form) throws CheckInException;

    public void updateCheckIn(String id, CheckInForm form) throws CheckInException;

    public void deleteCheckIn(String id) throws CheckInException;

    public void applyPatchOnCheckIn(String id, List<PatchOperationForm> patches) throws CheckInException;

}
