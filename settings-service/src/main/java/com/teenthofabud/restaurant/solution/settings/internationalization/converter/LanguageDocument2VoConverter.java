package com.teenthofabud.restaurant.solution.settings.internationalization.converter;

import com.teenthofabud.core.common.converter.TOABBaseDocument2VoConverter;
import com.teenthofabud.restaurant.solution.settings.internationalization.data.LanguageDocument;
import com.teenthofabud.restaurant.solution.settings.internationalization.data.LanguageVo;
import com.teenthofabud.restaurant.solution.settings.utils.SettingsServiceHelper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@Slf4j
public class LanguageDocument2VoConverter extends TOABBaseDocument2VoConverter<LanguageDocument, LanguageVo> implements Converter<LanguageDocument, LanguageVo> {

    private SettingsServiceHelper settingsServiceHelper;

    private List<String> fieldsToEscape;

    @Value("#{'${res.settings.language.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Autowired
    public void setSettingsServiceHelper(SettingsServiceHelper settingsServiceHelper){
        this.settingsServiceHelper = settingsServiceHelper;
    }

    @Override
    public LanguageVo convert(LanguageDocument entity) {

        LanguageVo vo = new LanguageVo();

        if(!fieldsToEscape.contains("id")) {
            vo.setId(entity.getId().toString());
        }
        if(!fieldsToEscape.contains("name")) {
            vo.setName(entity.getName());
        }
        if(!fieldsToEscape.contains("code")) {
            vo.setCode(entity.getCode());
        }
        if(!fieldsToEscape.contains("idiom")) {
            vo.setIdiom(entity.getIdiom());
        }
        if(!fieldsToEscape.contains("canDelete")) {
            vo.setCanDelete(entity.getCanDelete());
        }
        super.expandAuditFields(entity, vo);
        log.debug("Converted {} to {} ", entity, vo);
        return vo;
    }
}
