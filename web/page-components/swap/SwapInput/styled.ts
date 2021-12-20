import styled from 'styled-components';
import Image from '../../../components/Image';

export const SwapBlockBody = styled.div`
  display: flex;
  flex-flow: row nowrap;
  align-items: center;
  padding: 0.75rem 0.75rem 0.75rem 1rem;
  position: relative;

  input {
    color: #fcfcfc;
    position: relative;
    font-weight: 500;
    outline: none;
    border: none;
    flex: 1 1 auto;
    background-color: transparent;
    font-size: 24px;
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
    padding: 0;
    -moz-appearance: textfield;
  }
`;

export const Input = styled.input`
  color: #fcfcfc;
  position: relative;
  font-weight: 500;
  outline: none;
  border: none;
  flex: 1 1 auto;
  background-color: transparent;
  font-size: 24px;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
  padding: 0;
  -moz-appearance: textfield;
`;

export const SelectTokenButton = styled.button<{ isSelected?: boolean }>`
  -webkit-box-align: center;
  display: flex;
  align-items: center;
  font-weight: 600;
  background-color: transparent;
  color: rgb(255, 255, 255);
  border-radius: 12px;
  /*box-shadow: rgba(0, 0, 0, 0.08) 0 6px 10px;*/
  cursor: pointer;
  user-select: none;
  border: none;
  padding: 5px 12px;
  font-size: 16px;
  margin: 0 0.25rem;
  white-space: nowrap;
  outline: none !important;

  &:hover {
    background-color: #14142b;
  }

  ${({ isSelected }) =>
    isSelected &&
    `
border-radius: 24px;
  /*background-color: rgb(255, 255, 255);*/
`}

  span {
    transition: all 200ms;
    background-image: -webkit-gradient(
      linear,
      left top,
      right bottom,
      color-stop(0, #f80179),
      color-stop(1, #bb0691)
    );
    background-image: gradient(
      linear,
      left top,
      right bottom,
      color-stop(0, #f80179),
      color-stop(1, #bb0691)
    );
    color: transparent;
    -webkit-background-clip: text;
    background-clip: text;
    display: flex;
    align-items: center;
  }
`;

export const SelectTokenDropdown = styled.ul<{ isActive?: boolean }>`
  display: none;
  position: absolute;
  list-style: none;
  top: 51px;
  right: 16px;
  border-radius: 8px;
  background: #14142b;
  z-index: 22;
  padding: 10px 0;
  width: 200px;
  box-shadow: 0 8px 24px rgba(78, 75, 102, 0.2);

  ${({ isActive }) => isActive && 'display: block;'}
`;

export const SelectedTokenImage = styled(Image)`
  margin-right: 12px;
  width: 24px;
  height: 24px;
  border-radius: 50%;
  box-shadow: rgba(0, 0, 0, 0.08) 0 6px 10px;
`;

export const TokenDropdownItem = styled.li<{ isDisabled: boolean }>`
  display: flex;
  align-items: center;
  padding: 8px 20px;
  cursor: pointer;

  &:hover {
    background-color: #262338;
  }

  ${({ isDisabled }) =>
    isDisabled &&
    `
  pointer-events: none;
  opacity: 0.2;
`}
`;

export const TokenImage = styled(Image)`
  box-shadow: rgba(0, 0, 0, 0.08) 0 6px 10px;
  border-radius: 24px;
  margin-right: 20px;
`;

export const TokenSymbol = styled.span`
  font-size: 16px;
  line-height: 16px;
  margin: 0;
  font-weight: 500;
  display: block;
  color: #f7f7f7;
`;

export const TokenName = styled.span`
  margin: 0;
  font-weight: 300;
  font-size: 12px;
  display: block;
  color: #f7f7f7;
`;
