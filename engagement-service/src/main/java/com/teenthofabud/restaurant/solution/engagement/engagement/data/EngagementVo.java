package com.teenthofabud.restaurant.solution.engagement.engagement.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.teenthofabud.core.common.data.vo.TOABBaseVo;
import com.teenthofabud.restaurant.solution.engagement.booking.data.BookingVo;
import com.teenthofabud.restaurant.solution.engagement.integration.establishmentarea.data.TableVo;
import lombok.*;

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
    private String bookingId;
    @ToString.Include
    private BookingVo booking;
    @ToString.Include
    private String tokenNumber;
    @ToString.Include
    private Integer noOfPersons;
    @ToString.Include
    private String tableId;
    @ToString.Include
    private TableVo table;
    @ToString.Include
    private String extRef;
    @ToString.Include
    private String instructions;


    @Override
    public int compareTo(EngagementVo o) {
        return Integer.compare(this.bookingId.compareTo(o.getBookingId()), this.tokenNumber.compareTo(o.getTokenNumber()));
    }
}
