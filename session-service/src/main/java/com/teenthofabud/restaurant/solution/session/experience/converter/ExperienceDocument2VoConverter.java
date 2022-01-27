package com.teenthofabud.restaurant.solution.session.experience.converter;

import com.teenthofabud.core.common.converter.TOABBaseDocument2VoConverter;
import com.teenthofabud.restaurant.solution.session.experience.data.ExperienceDocument;
import com.teenthofabud.restaurant.solution.session.experience.data.ExperienceVo;
import com.teenthofabud.restaurant.solution.session.utils.SessionServiceHelper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@Slf4j
public class ExperienceDocument2VoConverter extends TOABBaseDocument2VoConverter<ExperienceDocument, ExperienceVo> implements Converter<ExperienceDocument, ExperienceVo> {

    private List<String> fieldsToEscape;
    private SessionServiceHelper sessionServiceHelper;

    @Value("#{'${res.session.experience.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }


    @Autowired
    public void setSessionServiceHelper(SessionServiceHelper sessionServiceHelper) {
        this.sessionServiceHelper = sessionServiceHelper;
    }

    @Override
    public ExperienceVo convert(ExperienceDocument entity) {
        ExperienceVo vo = new ExperienceVo();
        if(!fieldsToEscape.contains("id")) {
            vo.setId(entity.getId().toString());
        }
        if(!fieldsToEscape.contains("name")) {
            vo.setName(entity.getName());
        }
        if(!fieldsToEscape.contains("description")) {
            vo.setDescription(entity.getDescription());
        }
        super.expandAuditFields(entity, vo);
        log.debug("Converted {} to {} ", entity, vo);
        return vo;
    }

}
