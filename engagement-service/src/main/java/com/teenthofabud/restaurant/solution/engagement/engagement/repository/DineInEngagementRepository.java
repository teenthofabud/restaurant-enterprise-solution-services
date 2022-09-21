package com.teenthofabud.restaurant.solution.engagement.engagement.repository;

import com.teenthofabud.core.common.repository.TOABBaseMongoRepository;
import com.teenthofabud.restaurant.solution.engagement.engagement.data.DineInEngagementDocument;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface DineInEngagementRepository extends MongoRepository<DineInEngagementDocument, String>, TOABBaseMongoRepository {

    public List<DineInEngagementDocument> findByBookingId(String bookingId);

    public List<DineInEngagementDocument> findByBookingIdAndTableIdAndNoOfPersons(String bookingId, String tableId, Integer noOfPersons);

    public Boolean existsByBookingIdAndTokenNumberAndTableIdAndNoOfPersons(String bookingId, String tableId, Integer noOfPersons);

}
