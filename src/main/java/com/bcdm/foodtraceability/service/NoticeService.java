package com.bcdm.foodtraceability.service;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.bcdm.foodtraceability.entity.Notice;
import com.baomidou.mybatisplus.extension.service.IService;
import com.bcdm.foodtraceability.entity.SelectPageEntity;
import com.bcdm.foodtraceability.entity.UserModel;

import java.util.List;

/**
 * <p>
 * 通知服务类
 * </p>
 *
 * @author 王
 * @since 2022-02-12
 */
public interface NoticeService extends IService<Notice> {


    /**
     * 公司创建新的通知
     *
     * @param notice 通知信息
     * @return 创建成功的通知信息
     * @throws Exception 创建新的通知失败
     */
    Boolean companyCreate(Notice notice) throws Exception;
    /**
     * 管理员创建新的通知
     *
     * @param notice 通知信息
     * @return 创建成功的通知信息
     * @throws Exception 创建新的通知失败
     */
    Boolean managementCreate(Notice notice) throws Exception;

    /**
     * 获取最新的通知信息
     *
     * @param userModel 用户读取信息
     * @return 未读通知信息
     * @throws Exception 查询失败
     */
    List<Notice> getNewNotice(UserModel userModel) throws Exception;

    /**
     * 获取全部通知信息
     *
     * @param selectInfo 查询条件
     * @return 查询到的通知信息
     * @throws Exception 查询失败
     */
    IPage<Notice> getAllNotice(SelectPageEntity<Notice> selectInfo) throws Exception;

    /**
     * 修改通知信息
     *
     * @param notice 需要修改的通知信息
     * @return 修改结果
     * @throws Exception 修改信息失败
     */
    Boolean modifyNotice(Notice notice) throws Exception;
}
