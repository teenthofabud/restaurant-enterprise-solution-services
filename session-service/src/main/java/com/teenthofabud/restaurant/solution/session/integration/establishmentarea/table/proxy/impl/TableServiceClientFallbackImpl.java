package com.teenthofabud.restaurant.solution.session.integration.establishmentarea.table.proxy.impl;

import com.teenthofabud.restaurant.solution.session.integration.establishmentarea.table.proxy.TableServiceClient;
import com.teenthofabud.restaurant.solution.session.integration.establishmentarea.table.data.TableVo;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Component("tableServiceClientFallback")
@Slf4j
public class TableServiceClientFallbackImpl implements TableServiceClient {

    @Override
    public TableVo getTableDetailsById(String id) {
        log.debug("Falling back to default implementation of getting table details by id");
        return new TableVo();
    }
}
