package com.teenthofabud.restaurant.solution.booking.engagement.data;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.fasterxml.jackson.datatype.jsr310.ser.LocalDateSerializer;
import com.teenthofabud.core.common.data.vo.TOABBaseVo;
import com.teenthofabud.restaurant.solution.booking.association.data.AssociationVo;
import lombok.*;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString(onlyExplicitlyIncluded = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class EngagementVo extends TOABBaseVo implements Comparable<EngagementVo> {

    @EqualsAndHashCode.Include
    @ToString.Include
    private String id;
    @ToString.Include
    private String associationId;
    @ToString.Include
    private AssociationVo association;
    @ToString.Include
    private EngagementEvent event;
    @JsonSerialize(using = LocalDateSerializer.class)
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "dd-MM-yyyy") // date pattern referenced from res.booking.engagement.event.date.format
    @ToString.Include
    private LocalDate date;
    @JsonSerialize(using = LocalDateSerializer.class)
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "HH:mm:ss") // date pattern referenced from res.booking.engagement.event.time.format
    @ToString.Include
    private LocalTime time;
    @JsonSerialize(using = LocalDateSerializer.class)
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "dd-MM-yyyy HH:mm:ss") // date pattern referenced from res.booking.engagement.event.timestamp.format
    @ToString.Include
    private LocalDateTime timestamp;

    @Override
    public int compareTo(EngagementVo o) {
        return Integer.compare(this.associationId.compareTo(o.getAssociationId()), this.event.compareTo(o.getEvent()));
    }
}
