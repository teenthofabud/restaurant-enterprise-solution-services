package com.teenthofabud.restaurant.solution.encounter.meeting.repository;

import com.teenthofabud.core.common.repository.TOABSimpleEntityBaseRepository;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingEntity;

import java.time.LocalDateTime;
import java.util.Optional;

public interface MeetingRepository<T extends MeetingEntity> extends TOABSimpleEntityBaseRepository<T> {

    public Boolean existsByAccountIdAndSequenceAndCreatedOnBetween(String accountId, String sequence, LocalDateTime start, LocalDateTime end);

    public Optional<T> findBySequenceAndCreatedOnBetween(String sequence, LocalDateTime start, LocalDateTime end);

    public Boolean existsByAccountIdAndSequence(String accountId, String sequence);
}
