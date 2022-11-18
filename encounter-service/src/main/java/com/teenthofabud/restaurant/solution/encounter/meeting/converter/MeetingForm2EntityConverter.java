package com.teenthofabud.restaurant.solution.encounter.meeting.converter;

import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingEntity;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.convert.converter.Converter;

import java.util.List;

@Slf4j
public abstract class MeetingForm2EntityConverter<T extends MeetingForm, U extends MeetingEntity>
        implements Converter<MeetingForm, MeetingEntity> {

    private List<String> fieldsToEscape;

    @Value("#{'${res.encounter.meeting.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public MeetingEntity convert(MeetingForm form) {
        MeetingEntity entity = new MeetingEntity();
        if(!fieldsToEscape.contains("sequence")) {
            entity.setSequence(form.getSequence());
        }
        if(!fieldsToEscape.contains("accountId")) {
            entity.setAccountId(form.getAccountId());
        }
        entity.setActive(Boolean.TRUE);
        log.debug("Converting {} to {}", form, entity);
        U child = this.convertChild((T) form, entity);
        return child;
    }

    protected abstract U convertChild(T heckInFormChild, MeetingEntity checkInEntity);

}
