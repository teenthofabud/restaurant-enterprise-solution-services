package com.teenthofabud.restaurant.solution.engagement.engagement.repository;

import com.teenthofabud.core.common.repository.TOABBaseMongoRepository;
import com.teenthofabud.restaurant.solution.engagement.engagement.data.TakeAwayEngagementDocument;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface TakeAwayEngagementRepository extends MongoRepository<TakeAwayEngagementDocument, String>, TOABBaseMongoRepository {

    public List<TakeAwayEngagementDocument> findByBookingId(String bookingId);

    public List<TakeAwayEngagementDocument> findByInstructions(String instructions);

    public List<TakeAwayEngagementDocument> findByBookingIdAndInstructions(String bookingId, String instructions);

    public Boolean existsByBookingIdAndInstructions(String bookingId, String instructions);

}
