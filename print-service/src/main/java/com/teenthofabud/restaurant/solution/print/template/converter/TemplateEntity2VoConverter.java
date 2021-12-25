package com.teenthofabud.restaurant.solution.print.template.converter;

import com.teenthofabud.core.common.converter.TOABBaseEntity2VoConverter;
import com.teenthofabud.restaurant.solution.print.template.data.TemplateEntity;
import com.teenthofabud.restaurant.solution.print.template.data.TemplateVo;
import com.teenthofabud.restaurant.solution.print.utils.PrintServiceHelper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@Slf4j
public class TemplateEntity2VoConverter extends TOABBaseEntity2VoConverter<TemplateEntity, TemplateVo> implements Converter<TemplateEntity, TemplateVo> {

    private List<String> fieldsToEscape;
    private PrintServiceHelper printServiceHelper;

    @Value("#{'${res.print.template.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }


    @Autowired
    public void setPrintServiceHelper(PrintServiceHelper printServiceHelper) {
        this.printServiceHelper = printServiceHelper;
    }

    @Override
    public TemplateVo convert(TemplateEntity entity) {
        TemplateVo vo = new TemplateVo();
        if(!fieldsToEscape.contains("id")) {
            vo.setId(entity.getId().toString());
        }
        if(!fieldsToEscape.contains("name")) {
            vo.setName(entity.getName());
        }
        if(!fieldsToEscape.contains("description")) {
            vo.setDescription(entity.getDescription());
        }
        if(!fieldsToEscape.contains("content")) {
            vo.setContent(entity.getContent());
        }
        if(!fieldsToEscape.contains("templateTypeId")) {
            vo.setTemplateTypeId(entity.getTemplateTypeId());
        }
        super.expandAuditFields(entity, vo);
        log.debug("Converted {} to {} ", entity, vo);
        return vo;
    }

}
