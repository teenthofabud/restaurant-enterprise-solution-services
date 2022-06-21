package com.teenthofabud.restaurant.solution.reservation.engagement.repository.impl;

import com.teenthofabud.core.common.repository.TOABBaseMongoRepository;
import com.teenthofabud.restaurant.solution.reservation.booking.service.BookingService;
import com.teenthofabud.restaurant.solution.reservation.engagement.converter.EngagementDto2ChildDocumentConverter;
import com.teenthofabud.restaurant.solution.reservation.engagement.data.*;
import com.teenthofabud.restaurant.solution.reservation.engagement.factory.EngagementDocumentRepositoryFactory;
import com.teenthofabud.restaurant.solution.reservation.engagement.repository.EngagementRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Example;
import org.springframework.data.domain.ExampleMatcher;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Map;
import java.util.Optional;

@Component
@Slf4j
public class DefaultEngagementRepositoryImpl implements EngagementRepository {

    private EngagementDocumentRepositoryFactory engagementDocumentRepositoryFactory;
    private BookingService bookingService;
    private Map<String, ? extends TOABBaseMongoRepository> engagementTypeRepository;
    private EngagementDto2ChildDocumentConverter<EngagementDto, ? extends EngagementDocument> engagementDto2ChildDocumentConverter;

    @Autowired
    public void setEngagementDto2ChildDocumentConverter(EngagementDto2ChildDocumentConverter<EngagementDto, ? extends EngagementDocument> engagementDto2ChildDocumentConverter) {
        this.engagementDto2ChildDocumentConverter = engagementDto2ChildDocumentConverter;
    }

    @Autowired
    public void setEngagementTypeRepository(Map<String, ? extends TOABBaseMongoRepository> engagementTypeRepository) {
        this.engagementTypeRepository = engagementTypeRepository;
    }

    @Autowired
    public void setBookingService(BookingService bookingService) {
        this.bookingService = bookingService;
    }

    @Autowired
    public void setEngagementDocumentRepositoryFactory(EngagementDocumentRepositoryFactory engagementDocumentRepositoryFactory) {
        this.engagementDocumentRepositoryFactory = engagementDocumentRepositoryFactory;
    }

    @Override
    public List<? extends EngagementDocument> findAll(EngagementDto engagementDto, ExampleMatcher matcherCriteria) {
        Optional<? extends EngagementDocument> optionalEngagementDocument = engagementDto2ChildDocumentConverter.convert(engagementDto);
        Optional<? extends MongoRepository> optionalMongoRepository = engagementDocumentRepositoryFactory.getEngagementTypeRepository(engagementDto);
        Example<? extends EngagementDocument> engagementDocumentExample = Example.of(optionalEngagementDocument.get(), matcherCriteria);
        MongoRepository mongoRepository = optionalMongoRepository.get();
        List<? extends EngagementDocument> g = mongoRepository.findAll(engagementDocumentExample);
        return g;
    }

    @Override
    public Boolean existsByAnyOrAllOf(EngagementDto engagementDto, ExampleMatcher matcherCriteria) {
        Optional<? extends EngagementDocument> optionalEngagementDocument = engagementDto2ChildDocumentConverter.convert(engagementDto);
        Optional<? extends MongoRepository> optionalMongoRepository = engagementDocumentRepositoryFactory.getEngagementTypeRepository(engagementDto);
        Example<? extends EngagementDocument> engagementDocumentExample = Example.of(optionalEngagementDocument.get(), matcherCriteria);
        MongoRepository mongoRepository = optionalMongoRepository.get();
        List<? extends EngagementDocument> g = mongoRepository.findAll(engagementDocumentExample);
        return !g.isEmpty();
    }

}
