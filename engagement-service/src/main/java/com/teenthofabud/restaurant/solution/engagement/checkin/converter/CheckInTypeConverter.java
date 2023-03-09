package com.teenthofabud.restaurant.solution.engagement.checkin.converter;

import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.restaurant.solution.engagement.checkin.constants.CheckInType;
import com.teenthofabud.restaurant.solution.engagement.constants.EngagementErrorCode;
import lombok.extern.slf4j.Slf4j;

import javax.persistence.AttributeConverter;
import javax.persistence.Converter;
import java.util.stream.Stream;

@Converter(autoApply = true)
@Slf4j
public class CheckInTypeConverter implements AttributeConverter<CheckInType, String> {

    @Override
    public String convertToDatabaseColumn(CheckInType category) {
        if (category == null) {
            return null;
        }
        log.info("Convert checkInType {} to code {}", category, category.getCode());
        return category.getCode();
    }

    @Override
    public CheckInType convertToEntityAttribute(String code) {
        if (code == null) {
            return null;
        }
        CheckInType checkInType = Stream.of(CheckInType.values())
                .filter(c -> c.getCode().equals(code))
                .findFirst()
                .orElseThrow(() -> new TOABSystemException(EngagementErrorCode.ENGAGEMENT_ACTION_FAILURE,
                        new Object[]{ "attributeConversion", "No CheckInType found for " + code }));
        log.info("Convert code {} to checkInType {}", code, checkInType);
        return checkInType;
    }
}