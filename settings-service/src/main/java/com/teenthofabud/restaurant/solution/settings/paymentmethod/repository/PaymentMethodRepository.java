package com.teenthofabud.restaurant.solution.settings.paymentmethod.repository;

import com.teenthofabud.core.common.repository.TOABBaseMongoRepository;
import com.teenthofabud.restaurant.solution.settings.paymentmethod.data.PaymentMethodDocument;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface PaymentMethodRepository extends MongoRepository<PaymentMethodDocument, String>, TOABBaseMongoRepository {

    public Boolean existsByName(String name);

    public List<PaymentMethodDocument> findByNameContaining(String name);

}
