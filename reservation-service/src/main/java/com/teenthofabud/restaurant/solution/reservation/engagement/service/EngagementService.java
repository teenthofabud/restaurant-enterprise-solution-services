package com.teenthofabud.restaurant.solution.reservation.engagement.service;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.restaurant.solution.reservation.engagement.data.EngagementException;
import com.teenthofabud.restaurant.solution.reservation.engagement.data.EngagementForm;
import com.teenthofabud.restaurant.solution.reservation.engagement.data.EngagementVo;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;
import java.util.Set;

@Service
public interface EngagementService {

    public Set<EngagementVo> retrieveAllByNaturalOrdering();

    public EngagementVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws EngagementException;

    List<EngagementVo> retrieveAllMatchingDetailsByBookingId(String bookingId, Optional<TOABCascadeLevel> optionalCascadeLevel) throws EngagementException;

    public List<EngagementVo> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalBookingId,
                                                                   Optional<String> optionalTokenNumber,
                                                                   Optional<String> optionalTableId,
                                                                   Optional<String> optionalExtRef,
                                                                   Optional<String> optionalInstructions) throws EngagementException;

    public String createEngagement(EngagementForm form) throws EngagementException;

    public void updateEngagement(String id, EngagementForm form) throws EngagementException;

    public void deleteEngagement(String id) throws EngagementException;

    public void applyPatchOnEngagement(String id, List<PatchOperationForm> patches) throws EngagementException;

}
