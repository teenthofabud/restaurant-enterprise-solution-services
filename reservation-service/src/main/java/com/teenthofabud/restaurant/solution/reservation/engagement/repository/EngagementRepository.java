package com.teenthofabud.restaurant.solution.reservation.engagement.repository;

import com.teenthofabud.restaurant.solution.reservation.engagement.data.EngagementDocument;
import com.teenthofabud.restaurant.solution.reservation.engagement.data.EngagementDto;
import io.swagger.v3.oas.models.examples.Example;
import org.springframework.data.domain.ExampleMatcher;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface EngagementRepository {

    public List<? extends EngagementDocument> findAll(EngagementDto engagementDto, ExampleMatcher matcherCriteria);

    Boolean existsByAnyOrAllOf(EngagementDto engagementDto, ExampleMatcher matcherCriteria);
}
