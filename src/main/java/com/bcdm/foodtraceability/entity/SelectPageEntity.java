package com.bcdm.foodtraceability.entity;

import com.alibaba.fastjson.JSONObject;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@EqualsAndHashCode(callSuper = false)
@ToString
public class SelectPageEntity <T>{

    /**
     * 条\页
     */
    private IPage<T> pageInfo;

    /**
     * 查询的公司ID
     */
    private Integer companyId;

    /**
     * 查询的用户
     */
    private Integer userId;

    /**
     * 需要查询的名称（模糊查询）
     */
    private String selectName;

    /**
     * 判斷參數
     */
    private Integer checkParam;

    public SelectPageEntity(String jsonInfo) {
        JSONObject selectInfo = JSONObject.parseObject(jsonInfo);
        pageInfo = new Page<>();
        pageInfo.setCurrent(null == selectInfo.getInteger("pageNo") ? 0 : selectInfo.getLong("pageNo"));
        pageInfo.setSize( null == selectInfo.getInteger("pageSize") ? 0 : selectInfo.getLong("pageSize"));
        this.companyId = selectInfo.getInteger("companyId");
        this.userId = selectInfo.getInteger("userId");
        this.selectName = selectInfo.getString("selectName");
        this.checkParam = selectInfo.getInteger("checkParam");
    }
}
