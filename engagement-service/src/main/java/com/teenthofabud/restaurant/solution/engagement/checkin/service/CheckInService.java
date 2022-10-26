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
public interface CheckInService<T extends CheckInForm, V extends CheckInVo> {

    public Set<V> retrieveAllByNaturalOrdering();

    public V retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws CheckInException;

    public List<V> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalAccountId,
                                                                //Optional<String> optionalTableId,
                                                                Optional<String> optionalSequence,
                                                                Optional<String> optionalNotes) throws CheckInException;

    public V retrieveAllMatchingDetailsByCriteria(String sequence, String date) throws CheckInException;

    public String createCheckIn(T form) throws CheckInException;

    public void updateCheckIn(String id, T form) throws CheckInException;

    public void deleteCheckIn(String id) throws CheckInException;

    public void applyPatchOnCheckIn(String id, List<PatchOperationForm> patches) throws CheckInException;

}
