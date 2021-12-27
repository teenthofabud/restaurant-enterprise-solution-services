package com.teenthofabud.restaurant.solution.settings.device.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.teenthofabud.core.common.data.vo.TOABBaseVo;
import lombok.*;


@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString(onlyExplicitlyIncluded = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class DeviceVo extends TOABBaseVo implements Comparable<DeviceVo> {

    @EqualsAndHashCode.Include
    @ToString.Include
    private String id;
    @ToString.Include
    private String name;
    @ToString.Include
    private String description;
    @ToString.Include
    private String deviceTypeId;
    @ToString.Include
    private String location;

    @Override
    public int compareTo(DeviceVo o) {
        return this.getName().compareTo(o.getName());
    }
}
