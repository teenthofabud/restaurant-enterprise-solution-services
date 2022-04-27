package com.teenthofabud.restaurant.solution.reservation.booking.repository;

import com.teenthofabud.core.common.repository.TOABBaseMongoRepository;
import com.teenthofabud.restaurant.solution.reservation.booking.data.BookingDocument;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.List;

@Repository
public interface BookingRepository extends MongoRepository<BookingDocument, String>, TOABBaseMongoRepository {

    public List<BookingDocument> findByCategoryId(String categoryId);

    public Boolean existsByTimestampAndAccountId(LocalDateTime timestamp, String accountId);

}
