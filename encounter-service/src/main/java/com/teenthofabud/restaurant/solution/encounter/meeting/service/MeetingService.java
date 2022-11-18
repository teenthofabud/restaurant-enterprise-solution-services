package com.teenthofabud.restaurant.solution.encounter.meeting.service;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingException;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingForm;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingVo;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;
import java.util.Set;

@Service
public interface MeetingService<T extends MeetingForm, V extends MeetingVo> {

    public Set<V> retrieveAllByNaturalOrdering();

    public V retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws MeetingException;

    public List<V> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalAccountId,
           Optional<String> optionalSequence, Optional<String> optionalNotes) throws MeetingException;

    public V retrieveAllMatchingDetailsByCriteria(String sequence, String date) throws MeetingException;

    public String createMeeting(T form) throws MeetingException;

    public void updateMeeting(String id, T form) throws MeetingException;

    public void deleteMeeting(String id) throws MeetingException;

    public void applyPatchOnMeeting(String id, List<PatchOperationForm> patches) throws MeetingException;

}
