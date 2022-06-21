package com.teenthofabud.restaurant.solution.reservation.engagement.converter;

import com.teenthofabud.core.common.converter.ComparativePatchConverter;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.restaurant.solution.reservation.engagement.data.EngagementDocument;
import com.teenthofabud.restaurant.solution.reservation.engagement.data.EngagementDto;
import com.teenthofabud.restaurant.solution.reservation.engagement.visitor.EngagementDto2DocumentAssigner;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.*;

@Service
public interface EngagementDto2ChildDocumentConverter<EngagementDto, T extends EngagementDocument> {

    public <T extends EngagementDocument> Optional<? extends EngagementDocument> convert(EngagementDto dto);

}