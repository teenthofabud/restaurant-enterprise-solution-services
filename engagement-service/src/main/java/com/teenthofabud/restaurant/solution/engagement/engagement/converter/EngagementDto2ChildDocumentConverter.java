package com.teenthofabud.restaurant.solution.engagement.engagement.converter;

import com.teenthofabud.restaurant.solution.engagement.engagement.data.EngagementDocument;
import org.springframework.stereotype.Service;

import java.util.*;

@Service
public interface EngagementDto2ChildDocumentConverter<EngagementDto, T extends EngagementDocument> {

    public <T extends EngagementDocument> Optional<? extends EngagementDocument> convert(EngagementDto dto);

}