package com.teenthofabud.restaurant.solution.reservation.engagement.repository;

import com.teenthofabud.core.common.repository.TOABBaseMongoRepository;
import com.teenthofabud.restaurant.solution.reservation.engagement.data.DeliveryEngagementDocument;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface DeliveryEngagementRepository extends MongoRepository<DeliveryEngagementDocument, String>, TOABBaseMongoRepository {

    public List<DeliveryEngagementDocument> findByBookingId(String bookingId);

    public List<DeliveryEngagementDocument> findByExtRef(String extRef);

    public List<DeliveryEngagementDocument> findByBookingIdAndExtRef(String bookingId, String extRef);

    public Boolean existsByExtRef(String extRef);

    public Boolean existsByBookingIdAndExtRef(String bookingId, String extRef);

}
