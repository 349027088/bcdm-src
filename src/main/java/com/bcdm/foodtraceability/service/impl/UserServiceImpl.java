package com.bcdm.foodtraceability.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.bcdm.foodtraceability.entity.*;
import com.bcdm.foodtraceability.exception.ServiceBusinessException;
import com.bcdm.foodtraceability.mapper.UserMapper;
import com.bcdm.foodtraceability.service.CompanyService;
import com.bcdm.foodtraceability.service.JurisdictionService;
import com.bcdm.foodtraceability.service.UserService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static com.bcdm.foodtraceability.common.Constants.*;
import static com.bcdm.foodtraceability.common.CreateMD5.Md5encode;
import static com.bcdm.foodtraceability.common.CreateUUID.getUUID;
import static com.bcdm.foodtraceability.common.HttpConstants.HTTP_RETURN_FAIL;
import static com.bcdm.foodtraceability.common.MessageConstants.*;

/**
 * <p>
 * 用户服务实现类
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
@Service
@Slf4j
public class UserServiceImpl extends ServiceImpl<UserMapper, User> implements UserService {

    private final JurisdictionService jurisdictionService;

    private final CompanyService companyService;

    public UserServiceImpl(JurisdictionService jurisdictionService, CompanyService companyService) {
        this.jurisdictionService = jurisdictionService;
        this.companyService = companyService;
    }

    @Override
    public Map<String, Object> login(User user) throws Exception {
        Map<String,Object> loginInfo = new HashMap<>();
        User loginUser = loginByUser(user);
        loginInfo.put("user",loginUser);
        Company companyByUser = companyService.getCompanyByUser(loginUser);
        if (null != companyByUser && COMPANY_STATUS_ON_SERVICE.equals(companyByUser.getCompanyStatus())) {
            loginInfo.put("company",companyByUser);
        }
        return loginInfo;
    }

    @Override
    public User register(User user) throws Exception {
        log.info(user.getLoginId() + "-------注册");
        QueryWrapper<User> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("login_id", user.getLoginId());
        if (0 == count(queryWrapper)) {
            user.setSalt(getUUID());
            String password = user.getPassword();
            user.setPassword(Md5encode(user.getPassword() + user.getSalt()));
            LocalDateTime now = LocalDateTime.now();
            user.setUserStatus(USER_STATUS_UNLOCK);
            user.setCreateTime(now);
            user.setUpdateTime(now);
            if (!save(user)) {
                throw new ServiceBusinessException(HTTP_RETURN_FAIL, REGISTER_FAIL);
            }
            user.setPassword(password);
            return loginByUser(user);
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, REGISTER_FAIL);
    }

    @Override
    public User modifyPassword(ModifyPassword userLoginInfo) throws Exception {
        log.info(userLoginInfo.getLoginId() + "-------修改密码");
        User targetUser = new User();
        BeanUtils.copyProperties(userLoginInfo, targetUser);
        targetUser = loginByUser(targetUser);
        String stringBuffer = userLoginInfo.getNewPassword() + targetUser.getSalt();
        targetUser.setPassword(Md5encode(stringBuffer));
        return getUser(targetUser, MODIFY_PASSWORD_FAIL);
    }

    @Override
    public User modifyUserInfo(User user) throws Exception {
        log.info(user.getLoginId() + "-------修改用户信息");
        return getUser(user, MODIFY_USERINFO_FAIL);
    }

    @Override
    public int lockUser(User user) throws Exception {
        log.info(user.getLoginId() + "-------锁定用户");
        user.setUserStatus(USER_STATUS_LOCK);
        UpdateWrapper<User> updateWrapper = forStatusUpdate(user);
        LocalDateTime now = LocalDateTime.now();
        user.setUpdateTime(now);
        if (update(user, updateWrapper)) {
            return USER_STATUS_LOCK;
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, LOCK_USER_FAIL);
    }

    @Override
    public int unLockUser(User user) throws Exception {
        log.info(user.getLoginId() + "-------解锁用户");
        user.setUserStatus(USER_STATUS_UNLOCK);
        UpdateWrapper<User> updateWrapper = forStatusUpdate(user);
        user.setUpdateTime(LocalDateTime.now());
        if (update(user, updateWrapper)) {
            return USER_STATUS_UNLOCK;
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, UNLOCK_USER_FAIL);
    }

    @Override
    public IPage<UserModel> getUserByCompany(SelectPageEntity<UserModel> selectInfo) throws Exception {
        List<UserModel> userList = jurisdictionGetUserList(selectInfo);
        if (SELECT_ZERO != userList.size()) {
            int start = (int) (selectInfo.getPageInfo().getSize() * (selectInfo.getPageInfo().getCurrent()-1L));
            int end = userList.size() >= selectInfo.getPageInfo().getSize() * (selectInfo.getPageInfo().getCurrent()) ?
                    (int) (selectInfo.getPageInfo().getSize() * (selectInfo.getPageInfo().getCurrent())) : userList.size();
            selectInfo.getPageInfo().setRecords(new ArrayList<>());
            selectInfo.getPageInfo().setTotal(userList.size());
            for (int i = start; i < end; i++) {
                selectInfo.getPageInfo().getRecords().add(userList.get(i));
            }
            return selectInfo.getPageInfo();
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, COMPANY_GET_USER_INFO_FAIL);
    }

    /**
     * 用户锁定解锁用共通处理
     *
     * @param user 用户
     * @return 生成的SQL操作
     */
    private UpdateWrapper<User> forStatusUpdate(User user) {
        UpdateWrapper<User> updateWrapper = new UpdateWrapper<>();
        updateWrapper
                .eq("user_id", user.getUserId())
                .eq("update_time", user.getUpdateTime())
                .set("user_status", user.getUserStatus())
                .set("update_time", LocalDateTime.now());
        return updateWrapper;
    }

    /**
     * 生成用户
     *
     * @param targetUser         查询出来的用户信息
     * @param modifyUserinfoFail 更改用户信息失败
     * @return 修改后的用户信息
     * @throws ServiceBusinessException 修改信息失败
     */
    private User getUser(User targetUser, String modifyUserinfoFail) throws ServiceBusinessException {
        UpdateWrapper<User> updateWrapper = new UpdateWrapper<>();
        updateWrapper
                .eq("user_id", targetUser.getUserId())
                .eq("update_time", targetUser.getUpdateTime());
        targetUser.setUpdateTime(LocalDateTime.now());
        if (update(targetUser, updateWrapper)) {
            return targetUser;
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, modifyUserinfoFail);
    }

    /**
     * 使用公司编号查找关联的所有用户信息
     *
     * @param selectInfo 需要查找用户的公司ID和員工種別
     * @return 公司的所有公司模板的用户信息
     * @throws Exception 获取关联信息失败
     */
    private List<UserModel> jurisdictionGetUserList(SelectPageEntity<UserModel> selectInfo) throws Exception {
        List<Jurisdiction> jurisdictionList = jurisdictionService.getJurisdictionByCompany(selectInfo.getCompanyId());
        List<UserModel> userList = new ArrayList<>();
        for (Jurisdiction jurisdiction : jurisdictionList) {
            if (COMPANY_USER_3.equals(selectInfo.getCheckParam()) && COMPANY_USER_3.equals(jurisdiction.getJurisdiction())){
                userAdd(selectInfo, userList, jurisdiction);
            }
            if (!COMPANY_USER_3.equals(selectInfo.getCheckParam()) && !COMPANY_USER_3.equals(jurisdiction.getJurisdiction())){
                userAdd(selectInfo, userList, jurisdiction);
            }

        }
        return userList;
    }

    /**
     * 用户查询筛选
     *
     * @param selectInfo 筛选条件
     * @param userList 查询结果
     * @param jurisdiction 关联信息
     */
    private void userAdd(SelectPageEntity<UserModel> selectInfo, List<UserModel> userList, Jurisdiction jurisdiction) {
        QueryWrapper<User> userQueryWrapper = new QueryWrapper<>();
        userQueryWrapper.eq("user_id", jurisdiction.getUserId());
        if (!StringUtils.isEmpty(selectInfo.getSelectName())){
            userQueryWrapper.likeRight("user_name",selectInfo.getSelectName());
        }
        User user = getOne(userQueryWrapper);
        if (null!=user){
            UserModel userModel = createUserModel(user, jurisdiction);
            userList.add(userModel);
        }
    }

    /**
     * 创造一个前端显示的用户模板
     *
     * @param user         查找出来的用户信息
     * @param jurisdiction 查找出来该用户的关联信息
     * @return 创建的用户显示模板
     */
    private UserModel createUserModel(User user, Jurisdiction jurisdiction) {
        UserModel userModel = new UserModel();
        BeanUtils.copyProperties(user, userModel);
        userModel.setIdentity(jurisdiction.getIdentity());
        userModel.setJurisdictionUpdateTime(jurisdiction.getUpdateTime());
        return userModel;
    }

    /**
     * 按照用户的账号密码获取用户信息
     *
     * @param user 储存用户的账号密码信息
     * @return 查找结果的用户信息
     * @throws Exception 查找用户失败
     */
    private User loginByUser(User user) throws Exception {
        log.info(user.getLoginId() + "-------登录");
        QueryWrapper<User> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("login_id", user.getLoginId());
        User selectUser = getOne(queryWrapper);
        if (!(null == selectUser)) {
            StringBuilder stringBuilder = new StringBuilder();
            stringBuilder.append(user.getPassword());
            stringBuilder.append(selectUser.getSalt());
            if (Md5encode(stringBuilder.toString()).equals(selectUser.getPassword())) {
                return selectUser;
            }
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, LOGIN_FAIL);
    }

}
