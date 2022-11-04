package com.teenthofabud.restaurant.solution.engagement.checkin.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.service.TOABBaseService;
import com.teenthofabud.restaurant.solution.engagement.checkin.converter.*;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.*;
import com.teenthofabud.restaurant.solution.engagement.checkin.factory.CheckInBeanFactory;
import com.teenthofabud.restaurant.solution.engagement.checkin.mapper.CheckInEntitySelfMapper;
import com.teenthofabud.restaurant.solution.engagement.checkin.mapper.CheckInForm2EntityMapper;
import com.teenthofabud.restaurant.solution.engagement.checkin.repository.CheckInRepository;
import com.teenthofabud.restaurant.solution.engagement.checkin.validator.*;
import com.teenthofabud.restaurant.solution.engagement.constants.EngagementErrorCode;
import com.teenthofabud.restaurant.solution.engagement.utils.EngagementServiceHelper;
import org.springframework.stereotype.Service;

import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.Set;

@Service
public interface CheckInService<T extends CheckInForm, V extends CheckInVo, 
        A extends CheckInFormValidator, B extends CheckInFormRelaxedValidator, C extends CheckInDtoValidator,
        D extends CheckInRepository, E extends CheckInEntitySelfMapper, F extends CheckInForm2EntityMapper,
        G extends CheckInForm2EntityConverter, H extends CheckInEntity2VoConverter, I extends CheckInDto2EntityConverter> {

    static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();

    public String getContextCheckInType();
    
    public void setCheckInBeanFactory(CheckInBeanFactory checkInBeanFactory);

    public void setToabBaseService(TOABBaseService toabBaseService);

    public void setEngagementServiceHelper(EngagementServiceHelper engagementServiceHelper);

    public A getCheckInFormValidator();

    public B getCheckInFormRelaxedValidator();

    public C getCheckInDtoValidator();

    public D getCheckInRepository();

    public E getCheckInEntitySelfMapper();

    public F getCheckInForm2EntityMapper();

    public G getCheckInForm2EntityConverter();

    public H getCheckInEntity2VoConverter();

    public I getCheckInDto2EntityConverter();

    default Long parsePK(String id) throws CheckInException {
        Long checkInId = -1L;
        try {
            checkInId = Long.parseLong(id);
            //log.debug("Parsed id {} to checkIn id {} in numeric format", id, checkInId);
            if(checkInId <= 0) {
                throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        } catch (NumberFormatException e) {
            //log.error("Unable to parse checkIn id", e);
            //log.debug(CheckInMessageTemplate.MSG_TEMPLATE_CHECKIN_ID_INVALID.getValue(), id);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID, new Object[] { "id", id });
        }
        return checkInId;
    }

    public Comparator<V> getCheckInVoTypeComparator();

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
