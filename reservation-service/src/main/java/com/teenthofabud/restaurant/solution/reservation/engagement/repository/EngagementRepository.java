/*
package com.teenthofabud.restaurant.solution.reservation.engagement.repository;

import com.teenthofabud.core.common.repository.TOABBaseMongoRepository;
import com.teenthofabud.restaurant.solution.reservation.engagement.data.EngagementDocument;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface EngagementRepository extends MongoRepository<EngagementDocument, String>, TOABBaseMongoRepository {

    public List<EngagementDocument> findByBookingId(String bookingId);

    public Boolean existsByBookingIdAndTokenNumber(String bookingId, String tokenNumber);

}
*/
