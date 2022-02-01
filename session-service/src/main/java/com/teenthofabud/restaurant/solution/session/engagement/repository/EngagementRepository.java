package com.teenthofabud.restaurant.solution.session.engagement.repository;

import com.teenthofabud.core.common.repository.TOABBaseMongoRepository;
import com.teenthofabud.restaurant.solution.session.engagement.data.EngagementDocument;
import com.teenthofabud.restaurant.solution.session.engagement.data.EngagementEvent;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.stereotype.Repository;

import java.time.LocalDate;
import java.time.LocalTime;
import java.util.List;

@Repository
public interface EngagementRepository extends MongoRepository<EngagementDocument, String>, TOABBaseMongoRepository {

    public List<EngagementDocument> findByAssociationId(String associationId);

    public Boolean existsByAssociationIdAndEventAndDateAndTime(String associationId, EngagementEvent event, LocalDate date, LocalTime time);

}