import { ComponentFixture, TestBed } from '@angular/core/testing';

import { ReferenciasPageComponent } from './referencias-page.component';

describe('ReferenciasPageComponent', () => {
  let component: ReferenciasPageComponent;
  let fixture: ComponentFixture<ReferenciasPageComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ ReferenciasPageComponent ]
    })
    .compileComponents();

    fixture = TestBed.createComponent(ReferenciasPageComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
